template :: Html ()
template = do
    tt_ [ class_ "docutils literal" ] $ do
        span_ [ class_ "pre" ] $ do
            "foreign"
        " "
        span_ [ class_ "pre" ] $ do
            "export"
    " and "
    tt_ [ class_ "docutils literal" ] $ do
        span_ [ class_ "pre" ] $ do
            "foreign"
        " "
        span_ [ class_ "pre" ] $ do
            "import"
        " "
        span_ [ class_ "pre" ] $ do
            "ccall"
        " "
        span_ [ class_ "pre" ] $ do
            "\"wrapper\""
    "\n"
