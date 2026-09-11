module Cp8.Data.AddressBook.Validation where

import Prelude

import Cp8.Data.AddressBook
  ( Address
  , Person
  , PhoneNumber
  , PhoneType
  , address
  , person
  , phoneNumber
  )
import Data.Either (Either)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

data Field
  = FirstName
  | LastName
  | Street
  | City
  | State
  | Phones
  | Phone PhoneType

instance Show Field where
  show FirstName = "First Name"
  show LastName = "Last Name"
  show Street = "Street"
  show City = "City"
  show State = "State"
  show Phones = "Phone Numbers"
  show (Phone phoneType) = show phoneType

derive instance Eq Field

data Error = Error Field String

errorField :: Error -> Field
errorField (Error field _) = field

type Errors = Array Error

nonEmpty :: Field -> String -> V Errors String
nonEmpty field "" = invalid [ Error field ("Field '" <> show field <> "' cannot be empty") ]
nonEmpty _ value = pure value

validatePhoneNumbers
  :: Field -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field [] =
  invalid [ Error field ("Field '" <> show field <> "' must contain at least one value") ]
validatePhoneNumbers _ phones =
  traverse validatePhoneNumber phones

lengthIs :: Field -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
  invalid [ Error field ("Field '" <> show field <> "' must have length " <> show len) ]
lengthIs _ _ value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: Field -> Regex -> String -> V Errors String
matches _ regex value | test regex value = pure value
matches field _ _ = invalid
  [ Error field ("Field '" <> show field <> "' did not match the required format") ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address
    <$> nonEmpty Street a.street
    <*> nonEmpty City a.city
    <*> lengthIs State 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber
    <$> pure pn.type
    <*> matches (Phone pn.type) phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person
    <$> nonEmpty FirstName p.firstName
    <*> nonEmpty LastName p.lastName
    <*> validateAddress p.homeAddress
    <*> validatePhoneNumbers Phones p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
