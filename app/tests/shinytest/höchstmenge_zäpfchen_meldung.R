app <- ShinyDriver$new("../../")
app$snapshotInit("höchstmenge_zäpfchen_meldung")

app$setInputs(New_Substanz = "Abführender Tee offizinal")
app$setInputs(New_Substanz2 = "Abführender Tee offizinal")
app$setInputs(New_Substanz3 = "Abführender Tee offizinal")
app$setInputs(WS = "ABACAVIR")
app$setInputs(`dosierung-arzneitaxe` = "Albendazol")
app$setInputs(inTabset = "Hartfettmengenrechner")
app$setInputs(`Stückanzahl` = character(0))

app$setInputs(`Stückanzahl` = 29)

app$snapshot()
