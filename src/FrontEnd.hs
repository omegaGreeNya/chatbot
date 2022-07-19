module FrontEnd where
{-   (run) where


run :: (Front h) => h -> m ()
run h = do
   events <- getEvents h
   responces <- MapM respond events
   MapM_ sendResponce responces

class Front where
-}