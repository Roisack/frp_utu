# Functional Reactive Programming
## reactive-banana /w wx-haskell

Jarkko Hurme

Jesse Kaukonen

Joona Laine

Mats Rauhala

## 1. Otsikointi

Sisälmykset tähän mniin, kivasti riipastaan vaan.

## 2. Esipuhe

Tässä esitelmässä käymme läpi funktionaalisen reaktiivisen ohjelmoinnin (FRP) perusteet. Vertaamme FRP:n toimintaa etenkin Androidin käyttöliittymäkehitykseen. Lisänä esitämme muutamia esimerkkejä käyttäen Haskellin reactive-banana -kirjastoa liittäen sen wx-widgetsiin.

## 3. Katsaus tekniikoihin

### 3.1 Käsitteistä

* Reaktiivinen ohjelmointi tarkoittaa sitä, että muuttujat määritetään eräänlaisina viittauksina toisiin muuttujiin. Viitattujen muuttujien tilan vaihduttua tulee myös viittausta käyttävän muuttujan muuttua.
* Funktionaalinen reaktiivinen ohjelmointi (FRP) on tapa tehdä käyttöliittymäohjelmointia funktionaalisesti
* Reactive-banana on Haskell-kirjasto FRP:n toteuttamiseen
* wx-haskell on Haskell-kirjasto Wx-Widgetsin käyttämiseen
* Sivuvaikutus tarkoittaa muutosta, jota jonkin komponentin ei oleteta tekevän. Funktiossa, jossa on yksi input ja yksi output, tämä voisi tarkoittaa jonkin funktion tarvitseman ulkoisen tiedon muuttamista laskun aikana.

### 3.2 Historiaa

Eräitä FRP:n pioneereja ovat Conal Elliott sekä Paul Hudak, jotka kirjoittivat uudesta tavasta luoda funktionaalisia reaktiivisia käyttöliittymiä julkaisussaan Functional Reactive Animation (1997)[1]. He esittivät Functional Reactive Animationin (Fran), jonka pääasialliset käsitteet ovat "käyttäytyminen sekä tapahtumat": käyttäytyminen kuvaa reaktiivisija arvoja, ja tapahtumat mitä tahansa ehtoja, jotka voivat laueta monellakin eri tavalla. Vuonna 2001 Anthony Courtney ja Elliott kirjoittivat julkaisun "Genuinely Functional User Interfaces" [2], jonka suurimmat lisäykset paradigmaan olivat signaalifunktiot Fruit-kirjastossa. Nämä signaalifunktiot ovat eräänlaisia reaktiivisia rakenteita, jossa tapahtumat ja signaalit muodostavat input-output -suhteen: Voidaan ajatella funktion olevan laatikko, jonka sisään aika-pohjainen tapahtuma menee inputtina. Ulos tästä laatikosta tulee tämän funktion aikapohjainen tulos.

Genuinely Functiona User Interfaces -julkaisun jälkeen FRP:n yleiset käsitteet alkoivat olla nykyisellään. Signaalit eivät kuitenkään ole käytössä kaikissa FRP:n rajapinnoissa - esimerkiksi reactive-banana ei näitä sisällä. Tällä hetkellä FRP:n alueella on paljon aktiivista kehitystyötä.

Reactive-bananan ensimmäisen version julkaisi Heinrich Apfelmus vuonna 2011. Sen uusin versio 0.7.0.0 julkaistiin elokuussa 2012.

### 3.3 Mitä vaatii toimiakseen

Reactive-banana ei itsessään tarjoa mitään konkreettista käyttöliittymää - se vain mahdollistaa FRP:n toteuttamisen Haskellissa. Kehittynein liitäntä on wx-haskell, joka mahdollistaa wx-widgetsin käyttämisen Haskellin kanssa. Käytimme edellämainittua kirjastoa reactive-bananan kanssa ja toteutimme sillä yksinkertaisia käyttöliittymiä.

Haskell käännetään jollakin Haskell-kääntäjällä binääri-muotoon monen muun kielen tavoin, jonka jälkeen sen suorittaminen tapahtuu samoin kuten muidekin valitun alustan binääreiden. Kääntäjistä suosituin on Glasgow Haskell Compiler.

Yleisesti kehitystyökalujen asennus mille tahansa Haskell-ympäristölle tehdään HackageDB:n kautta käyttäen Cabal-ohjelmaa. Cabal pyrkii muiden paketinhallintajärjestelmien tavoin selvittämään muut vaadittavat paketit ja asentamaan ne ennen haluttua pakettia. Esimerkiksi wx-haskell ja reactive-banana asennus cabalin läpi voidaan suorittaa seuraavasti:

>cabal update

>cabal install reactive-banana

>cabal install reactive-banana-wx


Koska kysessä on vain korkean tason FRP-kirjasto, niin ei itse reactive-banana rajoita alustan valintaa millään tavalla. Sen sijaan valittu käyttöliittymäkirjasto määrää nämä rajoitteet - esimerkiksi wx-haskell tukee kaikkia niitä alustoja, joissa wx-widgets toimii.

### 3.4 Miten ohjelmat ilmaistaan

FRP:n ytimenä toimivat aika-vaikutteiset muuttujat: Kun jokin arvo asetetaan johonkin relaatioon, niin sen voidaan aina sanoa varmasti olevan tässä relaatiossa. Epä-reaktiivisessa ympäristössä tätä ei tapahtuisi, vaan jokainen muuttuja määriteltäisiin johonkin konkreettiseen pysyvään arvoon. Tämä asetettu arvo ei muuttuisi mihinkään ennen kuin jokin sitä muokkaava taho kävisi tämän operaation suorittamassa. Funktionaalinen reaktiivinen ohjelmointi on siis erittäin sopiva tapa ilmaista reaktiivisuutta, sillä funktionaaliset kielet ovat valtaosin reaktiivisia jo perustaltaan. Esimerkkinä hiiren määrittäminen ajan suhteen voitaisiin määrittämällä relaatio:

~~~~{.haskell}
position <- mouse.position
~~~~

Millä tahansa ajan hetkellä position olisi varmasti hiiren sen-hetkinen sijainti ilman, että tätä käytäisiin millään tavalla käsittelemässä manuaalisesti.

Reactive-bananan kehittäjä antaa esimerkkinä taulukkolaskennan[3]:

Jos oletetaan, että taulukkolaskennassa halutaan selvittää tuotteiden arvolisällisiä arvoja, niin tehokkain tapa on määrittää oma kenttä ALV:lle. Jos tuotteet verottomilla arvoilla ovat kolumnissa A soluissa A1 - A10, ja jokaisen tuotteen ALV on vieressä B-soluissa, niin voisimme yksinkertaisesti määrittää relaation:

C1 = A1*(1 + B1 / 100)

Jos ALV muuttuisi myöhemmin, niin pitäisi taulukon jokaisen arvolisäverollisen hinnan muuttua heti ALV-kentän muututtua.

Funktionaaliset kielet ovat ytimessään sivuvaikutuksettomia, mutta usein sivuvaikutukset ovat välttämättömiä käyttöliittymissä: Hiirtä on saatettu painaa muutama minuutti sitten, ja tämän vaikutukset näkyvät kenties vasta nyt käyttöliittymässä. Funktionaaliset kielet ovat yleisesti ratkaisuja ongelmiin määriteltynä käsitteen "nyt, tämä hetki" kautta, joten FRP:n täytyy kiertää tätä ongelmaa. Tämä on usein ratkaistu pitämällä pitkää historiaa tapahtumista, mutta ongelmaksi muodostuvat mahdollisesti suureksi kasvavat historiat. Liian suuri muistinkäyttö voi aiheuttaa hitautta, jos ongelma kasvaa liian suureksi. Imperatiivissa ympäristöissä tällaisia historioita ylläpidetään käsin, mutta tämä on usein vaikea tehtävä: tuloksena voi olla muistivuotoja, etenkin moniajoa hyödyntävissä ohjelmissa[4].

## 4 Työkalut

Ei suuremmin ole vielä mitään olemassa. jotain juttua ghc:stä ja ehkäpä kuva siitä ainoasta haskell-idestä.

## Lähteet:

[1]: Elliot, Hudak: Functional Reactive Animation http://conal.net/papers/icfp97/

[2]: Courtney, Elliot: Genuinely Functional User Interfaces http://webdoc.sub.gwdg.de/ebook/serien/ah/UU-CS/2001-62.pdf#page=47

[3]: http://www.haskell.org/haskellwiki/FRP_explanation_using_reactive-banana

[4]: http://c2.com/cgi/wiki?FunctionalReactiveProgramming
