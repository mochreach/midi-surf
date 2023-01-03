module Utils exposing (mmap6)


mmap6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Maybe a
    -> Maybe b
    -> Maybe c
    -> Maybe d
    -> Maybe e
    -> Maybe f
    -> Maybe value
mmap6 func ma mb mc md me mf =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case mb of
                Nothing ->
                    Nothing

                Just b ->
                    case mc of
                        Nothing ->
                            Nothing

                        Just c ->
                            case md of
                                Nothing ->
                                    Nothing

                                Just d ->
                                    case me of
                                        Nothing ->
                                            Nothing

                                        Just e ->
                                            case mf of
                                                Nothing ->
                                                    Nothing

                                                Just f ->
                                                    Just (func a b c d e f)
