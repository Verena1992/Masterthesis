app <- ShinyDriver$new("../../")
app$snapshotInit("addRezeptur")

app$setInputs(`jun_and_int-neue_rzs` = "click")
app$setInputs(New_Substanz = "Abführender Tee offizinal")
app$setInputs(New_Substanz2 = "Abführender Tee offizinal")
app$setInputs(New_Substanz3 = "Abführender Tee offizinal")
app$setInputs(`dosierung-arzneitaxe` = "Albendazol")
app$setInputs(inTabset = "Rezeptursammlung")
app$setInputs(inTabset = "neue_Zusammensetzung_Rezeptur")
app$setInputs(`Zusammensetzung-1` = "Noscapin")
app$setInputs(`Zusammensetzung-1num` = 0.005)
app$setInputs(`Zusammensetzung-2` = "Hartfett")
app$setInputs(`Zusammensetzung-2ad` = "click")
app$waitForValue("Zusammensetzung-2ad", ignore = list(NULL))
app$snapshot()
app$setInputs(`Zusammensetzung-jump_2_Herstellungshinweise` = "click")
app$setInputs(`textAreas-Titel` = "Herstellvorschrift für Noscapin HCl Zäpfchen als Ersatz für Tuscalman A Supp 1 g")
app$setInputs(`textAreas-Quelle` = "Österreichische Apothekerkammer, gespeichert von Verena")

app$setInputs(`textAreas-Herstellungshinweise` = "Tipps für eine bessere Genauigkeit
• Überschuss einplanen
• Zum Schmelzen des Hartfettes Wasserbad-Temperatur nicht zu hoch wählen max. 60°C
• Nach Zugabe des Wirkstoffes die Masse gut rühren bis dieser dispergiert ist und in der
Schmelze erscheint. Dieser Vorgang ist nicht mehr am Wasserbad durchzuführen.

Allerdings wird von einer Kleinproduktion à 10 Stück abgeraten, da die Dosiergenauigkeit nicht gewährleistet werden kann und es sehr leicht zu Abweichungen kommen kann.

Daher wird eine Herstellung von 30 bis 50 Stück empfohlen.")
app$snapshot()
app$setInputs(eigeneRezeptur_hinzu = "click")
app$setInputs(inTabset = "Home")
app$snapshot()
#app$snapshotDownload("download_newRezeptur")

