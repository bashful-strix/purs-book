module Cp8.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import CSS (backgroundColor, color, rgb, textColor)

import Data.Array (filter, mapWithIndex, updateAt)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HD
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Cp8.Data.AddressBook (Person, examplePerson)
import Cp8.Data.AddressBook.Validation
  ( Error(..)
  , Field(..)
  , errorField
  , validatePerson'
  )

main :: Effect Unit
main = do
  log "Rendering address book component"
  HA.runHalogenAff $ HA.awaitBody >>= runUI addressBook unit

data Action = UpdatePerson (Person -> Person)

addressBook :: ∀ q i o m. H.Component q i o m
addressBook = H.mkComponent
  { initialState: \_ -> examplePerson
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = case _ of
          UpdatePerson f -> H.modify_ f
      }
  }

  where
  render person =
    let
      formField = formField' case validatePerson' person of
        Left e -> e
        Right _ -> []

    in
      HD.div []
        [ HD.div []
            [ HD.form [] $
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

                , HD.h3 [] [ HD.text "Contact Information" ]
                ] <> (_ `mapWithIndex` person.phones) \index phone ->
                  formField (Phone phone.type) "XXX-XXX-XXXX" phone.number
                    \s p -> p
                      { phones = updateAt' index phone { number = s } p.phones }
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
