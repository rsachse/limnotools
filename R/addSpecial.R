"remSpecial" <-
function() {
    winMenuDelItem("Special", "Text Help")
    winMenuDelItem("Special", "HTML Help")
    winMenuDelItem("Special", "Windows Help")
    winMenuDelItem("Special", "remove this Menue")
    winMenuDel("Special")
}

"addSpecial" <-
function() {
    winMenuAdd("Special")
    winMenuAddItem("Special", "Text Help", "sethelp(1)")
    winMenuAddItem("Special", "HTML Help", "sethelp(2)")
    winMenuAddItem("Special", "Windows Help", "sethelp(3)")
    winMenuAddItem("Special", "remove this Menue", "remSpecial()")
}



