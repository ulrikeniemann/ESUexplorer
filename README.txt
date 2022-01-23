*******************************************************************************
* ESU explorer
* Ulrike Niemann
*******************************************************************************

Der ESU explorer ist eine Anwendung zur Analyse der Daten der Einschulungsuntersuchungen in Berlin-Mitte der Schuljahre 2010/2011 bis 2019/2020.
Der ESU explorer wurde entwickelt von Ulrike Niemann im Rahmen einer Masterarbeit zum Thema Explorative Datenanalyse mit R Shiny.


*******************************************************************************
* Installationsanweisung
*******************************************************************************

1) benötigte Programme
----------------------

1.1) R muss installiert sein
Download R: https://cloud.r-project.org/

1.2) R-Studio muss installiert sein
Download R-Studio: https://rstudio.com/products/rstudio/download/#download

1.3) nur für MAC OS: XQuartz muss zusätzlich installiert werden 
Download XQuartz: https://www.xquartz.org/ 

1.4) ein Internetbrowser eigener Wahl muss installiert sein
(Google Chrome, Mozilla Firefox, Safari o.a.)

*******************************************************************************
* Start und Beenden des Programms
*******************************************************************************

3) Start der Anwendung
----------------------

Im Anwendungsordner "ESUexplorer" die Datei "ESUexplorer.R" in R-Studio öffnen.
Oben rechts auf den Button "Run app" klicken um die Anwendung zu starten.
Die Anwendung öffnet sich im Internetbrowser.

Beim allerersten Laden werden alle benötigten Packages von R-Studio aus dem Internet heruntergeladen (eine bestehende Internetverbindung ist Voraussetzung). Dieser Prozess dauert einige Minuten! Bei einigen Packages kommt hier eventuell noch eine Frage ob Source oder Binary-Code installiert werden soll: hier können Sie mit Antwort No weitermachen ("n" in die Konsole hinter die Frage eintippen und Enter).

4) Schließen der Anwendung
--------------------------

Die Anwendung kann geschlossen werden über:
- Button "Anwendung schließen" ganz rechts oben in der Anwendung (der Tab des Internetbrowsers wird auch geschlossen)
- Schließen des Tabs im Internetbrowser
- Schließen des Internetbrowsers
- Klick auf "Stop"-Symbol in R-Studio (in der Console)
- Drücken der ESC-Taste in R-Studio

HINWEIS:
Wenn durch einen Fehler das Programm "abstürzt" bzw. durch einen R-Error abgebrochen wird, muss es neu über "Run app" in R-Studio gestartet werden.


*******************************************************************************
* R-Packages
*******************************************************************************

Die benötigten R-Packages werden automatisch installiert, wenn diese auf dem Anwenderrecher noch nicht vorhanden sind.
Die Anwendung nutzt folgenden Packages:

R-Package (Version)
shiny (1.3.2)
shinydashboard (0.7.1)
shinyjs (1.0)
shinycssloaders (0.2.0)
tidyverse (1.2.1)
mapproj (1.2.6)
DT (0.8)
janitor (1.2.0)
ggiraph (0.7.0)
Kernelheaping (2.2.1)
scales (1.1.0)
Hmisc (4.2.0)
RColorBrewer (1.1.2)
openxlsx (4.1.0.1)
DescTools (0.99.28)
rlang (0.4.0)
officer (0.3.8)
rvg (0.2.4)
ggExtra (0.9)
