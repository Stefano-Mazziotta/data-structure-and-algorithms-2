type Name = String
data Call b = Beep Int | Text Name b (Call b) deriving Show

mycall = (Text "luis" "hola soy luis" (Text "pepe" "hola soy pepe" (Beep 10)))

lookfor::Name -> Call b -> Maybe [b]
lookfor username (Beep n) = Nothing
lookfor username (Text username2 msg call) =
    if username == username2
        then
            case (lookfor username call) of
                Nothing -> Just [msg]
                Just msgs -> Just (msg:msgs)
        else
            lookfor username call

oldest:: Call b -> Maybe b
oldest (Beep n) = Nothing
oldest (Text username msg (Beep n)) = Just msg
oldest (Text username msg call) = oldest call

kick:: Name -> Call b -> Call b
kick _ (Beep n) = (Beep n)
kick username (Text sender msg call)  
    | username == sender = kick username call
    | otherwise = (Text sender msg (kick username call))
