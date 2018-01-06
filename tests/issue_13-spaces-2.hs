template :: Html ()
template = do
    html_ $ do
        "\nstartPre\n"
        pre_ $ do
            "\n\n"
            i_ $ do
                "abc"
            "\n\n"
            b_ $ do
                "def"
            "\n\n"
        "\nendPre"
        br_ []
        "\nstartP\n"
        p_ $ do
            "\n\n"
            i_ $ do
                "1 \\n above, 3 \\n below"
            "\n\n\n\n"
            b_ $ do
                "1 \\n below"
            "\n\n"
        "\nendP"
        br_ []
        "\n3nbsp\160\160\160here\n"
    "\n"
