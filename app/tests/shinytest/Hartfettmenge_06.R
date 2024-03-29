app <- ShinyDriver$new("../../")
app$snapshotInit("Hartfettmenge_06")

app$setInputs(New_Substanz = "Abführender Tee offizinal")
app$setInputs(New_Substanz2 = "Abführender Tee offizinal")
app$setInputs(New_Substanz3 = "Abführender Tee offizinal")
app$setInputs(`dosierung-arzneitaxe` = "Albendazol")
app$uploadFile(`nrf_and_int-Verdrängungsfaktoren` = "anlage-f_el2021-1_2800.pdf")
app$setInputs(inTabset = "Hartfettmengenrechner")
app$setInputs(WS_S = "Substanz nicht in Liste vorhanden")
app$setInputs(New_Substanz = "Aciclovir")
app$setInputs(`New_Verdrängungsfaktor` = 0.6)
app$setInputs(`Substanz_hinzufügen` = "click")
app$setInputs(`Stückanzahl` = 10)
app$setInputs(Eichwert = 1.9)
app$setInputs(`Überschuss` = 20)
app$setInputs(Menge_Substanz1 = 0.03)
app$snapshot()
app$setInputs(Berechnung_Menge = "click")
app$snapshot()
