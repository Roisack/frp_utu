# Käyttöliittymien ryhmätyö
## Jarkko Hurme, Jesse Kaukonen, Joona Laine, Mats Rauhala

## Tehtävänanto

Tehtävämme oli toteuttaa graafinen ohjelma opiskelija- sekä kurssitietojen esittämiseen. Ohjelman tuli pystyä lajittelemaan tietoa tiettyjen parametrien mukaan sekä listata opiskelijoita suoritusten määrän mukaan. Kiinnostaviksi kohteiksi piti tulkita etenkin hyvin opinnoissaan menestyvät opiskelijat.

## Valittu tekniikka

Teimme selvitystyön funktionaalisesta reaktiivisesta ohjelmoinnista, joten päätimme soveltaa sitä tässä harjoitustyössä. Päätimme yksinkertaistaa ohjelmaa käyttämällä reactive-bananan sijaan verkkosivulla toimivaa ratkaisua, jossa bacon.js tarjosi FRP:n käyttämisen.

Ytimenä on Happstack, joka on Haskell-pohjainen verkkopalvelinkirjasto. Ohjelmamme käynnistää Happstackin prosessiin, joka jää odottamaan pyyntöjä tiettyyn porttiin. Ohjelma palvelee HTML-tiedostoja ulos, jotka kirjoitetaan blaze-html -Haskell-kirjaston avulla.

Webbisivulla käytämme jquerya HTML-elementtien muokkaamiseen. Itse tieto esitetään datatables -taulukossa. Bacon.js hoitaa tapahtumienkäsittelyn, ja tietoa taulukkoon saadaan Ajax-kyselyllä Happstackin kautta tarjottaviin palveluihin.

Ulkoasu on rakennettu Twitter Bootstrapin avulla.

## Tietotyypit

Seuraavat oleelliset tyypit määrittävät miten tietoa käsitellään:

* Student on kokoelma tyyppejä, jotka kuvaavat opiskelijaa.
* Credit kuvaa suorituksia
* Degree kuvaa koulutusohjelmaa
* Date on Tuple, joka antaa opiskeluajan vuotena ja vuodenaikana

## Funktiot

* parseCredits lukee suoritusten datan ja tallentaa muodostaa niistä Credits-tyypin mukaisia muuttujia
* parseStudents lukee opiskelijoiden listauksen ja muodostaa tästä datasta Student-tyypin muuttujia
* parseThesis lukee opinto-ohjelmien datan ja muodostaa niistä opintokokonaisuuksia Degree-tyyppiin
* studentModal tuottaa HTML-muotoon modaalin (dialogin), jossa esitellään lisätietoja valitusta tiedosta
* thesisQuery tarjoaa palvelun Ajaxin yli tietyn tutkinnon tarkasteluun, palauttaen opinto-ohjelman kurssisisällön
* studentQUery tarjoaa palvelun Ajaxin yli opiskelijan tietojen tarkasteluun, palauttaen opiskelijan suoritusten tiedot
* fileResponse 
* unionCoursesData 
* creditsData palauttaa tiedot suorituksista suoritustauluun
* thesisData palauttaa tiedot opinto-ohjelmista opinto-ohjelmatauluun
* studentsData palauttaa tiedot opiskelijoista opiskelijatauluun
* studentsUpload lähettää uudet opiskelijatiedot sivun datatableen
* thesisUpload lähettää uudet opinto-ohjelmatiedot sivun datatableen
* creditsUpload lähettää uudet suoritustiedot sivun datatableen
* notFoundView tuottaa HTML-sivun, joka vastaa virhettä 404
* uploadForm luo näkymättömän iframen, jonka kautta voidaan lähettää tietoja sivulle
* mainView tuottaa itse sivun HTML-koodin
* lookBSsafe
* looksafe
* studentsCredits palauttaa vain ne suoritukset, jotka tietty opiskelija on suorittanut
* main lataa datan sekä käynnistää tarjottavat palvelut

## Tarjotut palvelut

* /student?studentId=XXXX tarjoaa tietoja pyydetystä opiskelijasta
* /thesis?degreeId=XXXX tarjoaa tietoja pyydetystä opinto-ohjelmasta
* /course?courseId=XXXX tarjoaa tietoja pyydetystä kurssista
