## UI-sovellusten "resepti"

Sekä WxWidgets ja Android ovat pohjimmiltaan hyvin lähellä toisiaan
Käyttöliittymätekniikoiden kannalta. Reactive-banana tuo tapahtumienkäsittelyyn
Ja toiminnallisuuden kuvaamiseen uusia piirteitä ja Android tuo uusia haasteita
Mobiilin ympäristönsä johdosta.

Erittäin korkealta tasolta katsottuna käyttöliittymät koostuvat ohjaimista
Jotka liitetään jonkinlaisiin ulkoasuohjaimiin. Ohjaimet reagoivat tapahtumiin
Joihin reagoidaan ohjelman tasolla. Ohjaimet ovat eräänlaisia näkymiä joiden
Kautta käyttäjä vaikuttaa sovelluksen toimintaan. Näitä ovat mm. painikkeet ja
Tekstikentät. Ohjaimet ovat pitkälti samanlaisia kummassakin tapauksessa.

Omien komponenttien tekeminen on samanlaista kummassakin järjestelmässä, mutta
Kummassakin pitäisi olla tarpeeksi valmiita komponentteja kaikkiin tarpeisiin.
Jos komponentit ei riitä, voidaan tehdä aliluokkia valmiista ohjaimista.

Ulkoasuohjaimet määrittelevät miten ohjaimet sijoitellaan sovelluksessa.
Tällaisia ohjaimia ovat esimerkiksi WxWidgets / reactive-banana -yhdistelmässä
`margin`, `grid` ja `row` kombinaattorit. Osoitteessa [Arithmetic.hs][counter]
On esimerkki ohjainten käytöstä. Siinä luodaan kolme ohjainta jotka asetellaan
Yhdelle riville vierekkäin.

~~~~{.haskell}
Input1 <- entry f []
Input2 <- entry f []
Output <- staticText f []

Set f [layout := margin 10 $row 10 $
        [widget input1, label "+", widget input2,
        label "=", minsize (sz 40 20) $ widget output]
~~~~

Androidin ulkoasuohjaimet ovat samantapaisia, mutta sillä erotuksella, että
Mobiilissa käyttöliittymässä niiden käyttö on monesti hieman erilaista.
Monesti, varsinkin laitteissa joissa on pieni näyttö, yksinkertaiset
Käyttöliittymät ovat parempia. Esimerkkinä Androidissa käytetystä
Ulkoasuohjaimesta on horisontaalinen lineaarinäkymä, joka lajittelee ohjaimet
Riviin vierekkäin. Toinen ero muodostuu tavasta määritellä näkymät, joka
Androidissa on yleensä kuvattu XML-dokumentilla.

Enemmän eroavaisuuksia tulee tapahtumien käsittelyssä. Reactive-banana on
Funktionaaliseen reaktiiviseen ohjelmointiin perustuva kirjasto.
Funktionaalinen reaktiivinen ohjelmointi perustuu jatkuviin aikaan perustuviin
Funktioihin (vrt diskreetit funktiot). Ajatuksena on siis, että asioilla on
Jatkuvasti ajasti riippuva tila. Läheisempänä esimerkkinä voidaan ottaa
Excel-dokumentit jossa solun arvo voi riippua toisen solun arvosta, jolloin sen
Arvo päivittyy automaattisesti kun toisen solun arvo muuttuu.

Toisin sanoen reaktiivisessa ohjelmoinnissa ohjelma kuvataan deklaratiivisesti
Etukäteen. Esimerkissä [Arithmetic.hs][counter] luodaan käytökset kahdesta
Tekstikentästä ja näihin kahteen käytökseen perustuva tulos joka myöskin on
Käytös. Esimerkki on triviaali, mutta kirjasto sisältää paljon kombinaattoreita
Erilaisiin tarkoituksiin.

Android vastaa perinteisempää ohjelmointitapaa, mutta siinäkin on omia
Erikoisuuksiaan. Yksi mielenkiintoinen on automattiset pinopohjaiset ikkunat.
Android yleensä tukee vain yhtä näkymää kerrallaan jolloin automaattinen
Näkymienhallinta on erittäin kätevä. Toinen erikoisuus ovat aikeet, jotka ovat
Ajonaikaisia pyyntöjä omalta tai muilta prosesseilta. Tapahtumienkäsittely
Tapahtuu perinteisesti anonyymeilla luokilla. Tähän väliin voisi pistää
Maininnan anonyymeista funktioista.

Eroja löytyy myös siitä, millä alustoilla järjestelmät toimivat.
Android-ohjelmat toimivat nimensä mukaisesti ainoastaan Android-laitteissa.
Vaikka alla oleva kieli toimisikin muilla alustoilla, on käyttöliittymäkirjasto
Ja järjestelmä tarjolla ainoastaan Android-laitteissa.

WxWidgets / reactive-banana sen sijaan toimii lähes kaikilla ei-mobiileilla
Laitteilla ja käyttöjärjestelmillä. WxWidgets toimii Windows, Linux ja
Mac-käyttöjärjestelmissä. Reactive-banana ei ole varsinaisesti kytköksissä
WxWidgets-kirjastoon, mutta sisältää referenssi-implementaation siihen.
Reactive-bananaan on kehitetty myös kaksi erilaista proof-of-concept liitosta
Web-sivuja varten, jolloin sen käyttömahdollisuus laajeene myös mobiileihin
Laitteisiin. Jos oletetaan, että käytetään WxWidgets-kirjastoa, vaaditaan
Edellämainittu kirjasto valmiiksi käännettynä, GHC-kääntäjä haskellia varten ja
Liitoskirjasto haskellin ja WxWidgets-kirjaston välille.

[counter]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana-wx/src/Arithmetic.hs Arithmetic.hs
