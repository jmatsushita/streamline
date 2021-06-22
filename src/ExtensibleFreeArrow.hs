data FreeA eff a b where
    Pure :: (a -> b) -> FreeA eff a b
    Effect :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a1 b1 -> FreeA eff a2 b2 -> FreeA eff (a1, a2) (b1, b2)
    -- Apply -- | Arrow apply
    -- FanIn -- | Arrow Choice
    -- Spl -- | Arrow Choice
    
instance C.Category (FreeA eff) where
    id = Pure id
    (.) = flip Seq

instance Arrow (FreeA eff) where
    arr = Pure
    first f = Par f C.id
    second f = Par C.id f
    (***) = Par
    
compileA :: forall eff arr a0 b0. (Arrow arr) => (forall a b. eff a b -> arr a b) -> FreeA eff a0 b0 -> arr a0 b0
compileA exec = go
	where
        go :: forall a b . (Arrow arr) => FreeA eff a b -> arr a b
        go freeA = case freeA of
        Pure f -> arr f
        Seq f1 f2 -> go f2 C.. go f1
        Par f1 f2 -> go f1 *** go f2
        Effect eff -> exec eff

evalKleisliA :: forall m a b . ( Monad m ) => FreeA (Kleisli m) a b -> Kleisli m a b
evalKleisliA = go
	where
        go :: forall m a b . (Monad m) => FreeA (Kleisli m) a b -> Kleisli m a b
        go freeA = case freeA of
            Pure f -> Kleisli $ return . f
            Effect eff -> eff
            Seq f1 f2 -> go f2 C.. go f1
            Par f1 f2 -> go f1 *** go f2

liftK :: Monad m => (b -> m c) -> FreeA (Kleisli m) b c
liftK eff = Effect (Kleisli $ \x -> eff x)

    
data PrintX a b where
	Print :: PrintX Text ()

interpPrintX :: (MonadIO m) => PrintX a b -> FreeA (Kleisli m) a b
interpPrintX Print = liftK (\x -> liftIO $ T.putStrLn x)

interpPrintXToFile :: (MonadIO m) => PrintX a b -> FreeA (Kleisli m) a b
interpPrintXToFile Print = liftK (\x -> liftIO $ T.writeFile "output.txt" x)

type (w :>+: a) = Sum2 w a

printA :: (eff :>+: PrintX) => FreeA eff Text ()
printA = lftE Print

storeA :: (eff :>+: StoreX) => FreeA eff String ()
storeA = lftE Store

extensibleArrow :: (eff :>+: PrintX, eff :>+: StoreX) => FreeA eff Text ()
extensibleArrow = proc x -> do
    printA -< x
    storeA -< T.unpack x
    Pure id -< ()
    
runKleisli (evalKleisliA $ compileA (interpPrintX <#>
interpStoreXToFile) extensibleArrow) ("Extensible Arrow" :: Text)