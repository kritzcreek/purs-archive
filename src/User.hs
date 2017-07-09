module User where

import Protolude

import Persist (User(..), EntityField(UserEmail))
import Crypto.PasswordStore (makePassword, verifyPassword)
import Database.Persist.Sqlite

type Email = Text
type Password = ByteString

data RegistrationError
  = UsernameTaken

hashStrength :: Int
hashStrength = 18

registerUser :: Email -> Password -> SqlPersistM ()
registerUser email password = do
  hashed <- liftIO $ makePassword password hashStrength
  let newUser = User email hashed
  isUnique <- isNothing <$> checkUnique newUser
  if isUnique
    then void $ insert newUser
    else pure ()

checkUser :: Email -> Password -> SqlPersistM (Maybe Bool)
checkUser email password = do
  res <- selectFirst [UserEmail ==. email] []
  case map entityVal res of
    Nothing -> pure Nothing
    Just (User _ storedHash) ->
      pure (Just (verifyPassword password storedHash))
