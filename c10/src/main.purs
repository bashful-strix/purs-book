module Cp10.Main where

import Prelude

import CSS (backgroundColor, color, rgb, textColor)

import Data.Array (filter, length, mapWithIndex, updateAt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)

import Data.Argonaut
  ( Json
  , decodeJson
  , encodeJson
  , jsonParser
  , printJsonDecodeError
  , stringify
  )

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HD
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

import Cp10.Data.AddressBook (Person, examplePerson)
import Cp10.Data.AddressBook.Validation
  ( Error(..)
  , Field(..)
  , errorField
  , validatePerson'
  )
import Cp10.Effect.Alert (alert, confirm)
import Cp10.Effect.Storage (getItem, removeItem, setItem)

main :: Effect Unit
main = do
  log "Rendering address book component"

  storagePerson <- getItem "person"
  initialPerson <- case processItem storagePerson of
    Left err -> do
      alert $ "Error: " <> err <> ". Loading examplePerson"
      pure examplePerson
    Right person -> pure person

  HA.runHalogenAff $ HA.awaitBody >>= runUI addressBook initialPerson

processItem :: Json -> Either String Person
processItem item = do
  jsonString <- decodeJson item
    # lmap (\e -> "No string in local storage: " <> printJsonDecodeError e)
  j <- jsonParser jsonString
    # lmap ("Cannot parse JSON string: " <> _)
  decodeJson j
    # lmap (\e -> "Cannot decode Person: " <> printJsonDecodeError e)

data Action
  = UpdatePerson (Person -> Person)
  | SavePerson Event
  | ResetPerson MouseEvent

addressBook :: ∀ q o m. MonadEffect m => H.Component q Person o m
addressBook = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

  where
  handleAction = case _ of
    UpdatePerson f -> H.modify_ f

    SavePerson e -> do
      H.liftEffect $ preventDefault e
      validated <- H.gets validatePerson'
      H.liftEffect case validated of
        Left errs -> alert
          $ "There are " <> show (length errs) <> " validation errors."
        Right person -> do
          setItem "person" $ stringify $ encodeJson person
          log "saved"

    ResetPerson e -> do
      H.liftEffect $ preventDefault $ toEvent e
      confirmed <- H.liftEffect $ confirm "Reset person?"
      when confirmed do
        H.liftEffect $ removeItem "person"
        H.put examplePerson
        log "reset"

  render person =
    let
      errors = case validatePerson' person of
        Left e -> e
        Right _ -> []
      formField = formField' errors

    in
      HD.div []
        [ HD.form
            [ HE.onSubmit SavePerson ]
            [ HD.h3 [] [ HD.text "Basic Information" ]
            , formField FirstName "First Name" person.firstName
                \s -> _ { firstName = s }
            , formField LastName "Last Name" person.lastName
                \s -> _ { lastName = s }

            , HD.h3 [] [ HD.text "Address" ]
            , formField Street "Street" person.homeAddress.street
                \s -> _ { homeAddress { street = s } }
            , formField City "City" person.homeAddress.city
                \s -> _ { homeAddress { city = s } }
            , formField State "State" person.homeAddress.state
                \s -> _ { homeAddress { state = s } }

            , HD.div [] $
                [ HD.h3 [] [ HD.text "Contact Information" ]
                ] <> (_ `mapWithIndex` person.phones) \index phone ->
                  formField (Phone phone.type) "XXX-XXX-XXXX" phone.number
                    \s p -> p
                      { phones = updateAt' index phone { number = s } p.phones }

            , HD.button [ HE.onClick ResetPerson ] [ HD.text "Reset" ]
            , HD.button [ HP.type_ HP.ButtonSubmit ] [ HD.text "Save" ]
            ]

        ]

  formField' errors field placeholder value setValue =
    HD.div [] $
      [ HD.label [] [ HD.text $ show field ]
      , HD.div []
          [ HD.input
              [ HP.id $ show field
              , HP.placeholder placeholder
              , HP.value value
              , HE.onValueChange $ UpdatePerson <<< setValue
              ]
          ]
      ] <> renderValidationErrors (filter (eq field <<< errorField) errors)

  renderValidationErrors [] = []
  renderValidationErrors errs =
    [ HD.div []
        [ HD.ul [] $ errs <#> \(Error _ err) ->
            HD.li []
              [ HD.span
                  [ HC.style do
                      let red = rgb 200 0 0
                      backgroundColor red
                      color $ textColor red
                  ]
                  [ HD.text err ]
              ]
        ]
    ]

  updateAt' i x xs = fromMaybe xs (updateAt i x xs)
