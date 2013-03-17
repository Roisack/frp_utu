## Testaus

Elämme kehittyvän tietotekniikan aikakaudella. Tietotekniikka on vuosien saatossa muuttanut rooliaan ja tullut jokapäiväiseksi osaksi elämäämme. Muutokset tietotekniikkaa eivät yksinään rajoitu miten käytämme sitä, mutta myös siihen miten kehitämme ohjelmistoja. 
Ohjelmistokehitys on muuttunut huomattavasti viimevuosikymmenen aikana. Tämän seurauksena myös ohjelmistojen testaus on saanut enemmän huomiota  ja voidaan sanoa, että testausvaihe onkin yksi tärkeimmistä vaiheista ohjelmistokehityksessä ei ainoastaan rahallisesti löydettäessä virheen ohjelmistosta, jonka korjaus myöhemmin olisi maksanut korkean hinnan vaan myös mittaamaan ohjelmiston laatua.  (Hurme, 2011.) 
Tässä osiossa dokumenttia aion kertoa testauksesta FRP- paradigman kannalta sekä Android – alustan kannalta.

### Functional reactive programming

Kyseisen ohjelmointi paradigman monimuotoisuudesta ja sen laajuudesta johtuen sillä ei ole varsinaisia testausperiaatteita, jotka toimisivat täydellisesti kaikilla sen kirjastoilla esim. testaus methodi, jota voi hyödyntää reactive – bananassa ei välttämättä toimi Netwiressä.
Koodauksessa voidaan yleisellä tasolla tosin hyödyntää sitä, että kyseiset funktiot käyttäytyvät kuin matemaattiset funktiot, jonka seurauksena ne palauttavat saman arvon tilasta riippumatta. Tämä on yksi funktionaalisen kielen eduista, joka tekee testauksesta nopeampaa.   

### Esimerkkinä oheisen koodin testaaminen:

~~~~{.python}
def formatArgs(args: Array[String]) = args.mkString(“\n”)) 
 val result = formatArgs(Array(“yksi”,”kaksi”, “kolme”)) 
assert(result == “yksi\nkaksi\nkolme”)
~~~~

”Assert”-metodi tarkistaa onko muuttuja “result” sama kuin “yksi\nkaksi\nkolme”, ja palauttaa ”boolean”-tyyppisen arvon. Jos tulos oli sama, palautetaan ”true”. Jos tulos ei ollut sama, palautetaan ”false”, ja aiheutuu ”AssertionError”-poikkeus. 
Testauksessa voidaan myös hyödyntää yleisiä testaukseen liittyviä methodeja esim. blackbox – white box testausta.
FRP: ssä  on tosin huomioitava se, että toisin kuin olio-ohjelmointiin perustuvissa kielissä tietojen eheyttä ei varmisteta käännettäessä niitä, eli onko kyseisillä tiedoilla kaikki tarvittavat ominaisuudet vai ovatko ne puutteelliset, esim. puuttuuko avain jostain tietueesta tai luokasta? Tämän seurauksena on hyödyllistä tehdä erinäinen versio, jossa voidaan testata sekä varmentaa kyseinen asia. (stackoverflow) 
Valmistuneen ohjelman ja sen käyttöliittymän testaamisessa voidaan käyttää samoja periaatteita kuin olio – ohjelmoinnilla luotujen ohjelmistojen testauksessa. FRP - kielten käyttäminen koodaamisessa ei aiheuta sitä, että automaattisia testejä ei voisi suorittaa tai, että manuaalinen testaaminen olisi mahdotonta. 

Päätelmänä FRP – pohjaisesta testauksesta voi sanoa sen verran, että se on omalta osaltaan erittäin toimiva methodi testaus painotteisessa suunnittelussa, johtuen funktioiden selkeydestä sekä mahdollisuudesta toistaa tehdyt testit vaivattomasti.  
Tosin on myönnettävä, että testien luonti on helpompaa olio-ohjelmoinnissa, johtuen erilaisten diagrammien ja UML – kaavioiden hyödyntämisestä ohjelmoinnin tukena, josta tulen puhumaan enemmän seuraavassa kappaleessa joka käsittelee testausta Androidissa.  

### Android

Testaaminen Androidissa on hyvin erilaista verrattuna FRP – paradigmaan. Syitä tähän ovat ilmiselvästi Android järjestelmien populaarisuus, käyttäjien määrä sekä työkalujen määrä. Androidissa on tarjolla lukuisia testaus järjestelmiä niin kaupallisia kuin ilmaisia. Android itsessäänkin sisältää testaukseen suunnatun integroidun testausrunkon , joka auttaa testaamaan ohjelmiston jokaista osiota erilaisten ominaisuuksienavulla. 

Kuva 1. Androidin sisäinen testausjärjestelmä.
![Androidin sisäinen testausjärjestelmä.](images/android_testaus.png)
Kyseinen testausrunko sisältää mahdollisuuden luoda testaussalkkuja, jotka perustuvat JUnittiin. JUnitin avulla on mahdollista testata eri luokkia, rakentaa vale objekteja ja määrittää erinäisten komponenttien elinkaaren. JUnittiin perustuvassa testauksessa  luodaan  toisistaan eristäytyneitä luokkia, joista jokainen luokka testaa omaa osaansa ohjelmistosta. Luokat  toimivat myös säiliöinä samankaltaisille testaus methodeille.   (developer.android.com )

~~~~{.java}
package fi.androidkehitys.esimerkkiapp.test;
import junit.framework.Test;
import junit.framework.TestSuite;

    public class AllTests {

    /**
    * Testisalkku, missä kaikki testiluokat lisätään.
    * @return Test
    */
    public static Test suite() {
        TestSuite suite = new TestSuite(AllTests.class.getName());

        suite.addTestSuite(EsimerkkiTesti.class);

        return suite;
    }
}
~~~~

Huomioitava ominaisuus Androidin testausrunkossa on myös se, että sillä on mahdollista luoda luokkia, jotka testaavat komponenttien tasolla vain tiettyä osiota.
Testien ajo suoritetaan käyttämällä hyväksi testiajo luokkaa joka lataa , valmistaa, ja lopettaa jokaisen testin. Käyttäjän on tosin itse päätettävä mitä testiajo luokkaa käytettään kyseisissä testeissä.

Käyttöliittymän testaaminen Androisissa on hitusen työläämpää. johtuen toimintojen elinkaarista ja muista ominaisuuksista. Kyseiseen pulmaankin tosin  löytyy paljon työkaluja, joita voi käyttää hyödyksi esimerkiksi. black box – testaus robotiumilla.  (Mobiilikehitys.) 
Black box testauksella pystytään tekemään monimutkaisiakin käyttötapaustestejä ilman tarvetta käsitellä tai nähdä koodia, mutta yksinkertaisuus testeissä on yleensä parempi tavoite sillä monimutkaiset testit voivat  kaatua pienilläkin muutoksilla. 
Käyttöliittymää voi myös testata manuaalisesti esimerkiksi luomalla testitapukset UML- kaavion perusteella  tai sekvenssi –kaavion avulla. Kyseisistä diagrammeista voi luoda tarvittavat testi tapaukset, jotka ottavat huomioon myö kaikki käyttäjän mahdolliset toiminnot.  Ohessa esimerkki UML- pohjaisesta testi tapausten luonnista käyttäen esimerkkinä eTM – Electronic Town Meeting  ohjelmistoa.   (Hurme, 2011.)

Kuva 2. Esimerkki  eTM –ohjelman UML-kaaviosta.
[etm](images/etm.png)
Kuva 3. UML- kaavion pohjalta luodut sekä toteutut testi tapaukset.
[uml](images/uml.png)

Manuaalisen testauksen heikkoutena on sen hitaus mutta sen hyötynä on mahdollisuus keskittyä tiettyihin käyttöliittymän elementteihin sekä kyky toimia ihmismäisemmin eli niin sanotusti nähdä tuote kuluttujan näkökulmasta. 
Käyttöliittymän testauksessa on myös huomioitava käytettävyys , saatavuus sekä yleisimmät tottumukset, joita käyttäjillä on esimerkiksi. jos ikkunan reunassa on x , käyttäjä olettaa, että se sulkee kyseisen ikkunan. Android sovelluksen esteettömyyden testauksessa olisi hyvä huomioida seuraavat yksityiskohdat. Kaikkien toimintojen pitäisi antaa selkeä palaute käyttäjälle sekä aiheuttaa kyseiseen toimintaan liittyvän tapahtuman. Käyttäjän pitäisi pystyä navigoimaan käyttöjärjestelmää helpon oloisesti, useilla eri navigointi vaihtoehdoilla. On myös varmistettava, että kaikille toiminnoilla on useita vaihtoehtoja eikä ainoastaan yhtä esimerkiksi. äänikomento tai ruudun koskettaminen voivat aiheuttaa saman toiminnon.
Androidilla on myös joitain testaus  suosituksia, joita voi noudattaa koskien käytettävyyttä. (developer.android.com )
* On suositeltavaa, että äänikomennot eroavat toisistaan hyvin, että ne eroavat toisistaan jokaista etsittävää tietoa varten.
* Liiallinen ja liian vähäinen äänikomentojen käyttäminen. Molemmat näistä tapahtumista voivat aiheuttaa ongelmia esimerkiksi, että ohjelmistossa on liian monia äänikomentoja voi vaikeuttaa sovelluksen käyttämistä. 
* Videoissa on oltava myös tekstitys huonokuuloisten henkilöiden varalle.

Päätelmänä voisin mainita, että Androidin testauksen heikkoutena on testien laatimisen pituus. Kyseinen prosessi vie oman aikansa ja ainakin vasta-alkajille kyseinen testaus prosessin luonti on haastavaa. Heikkoutena  voi mainita myös sen , että kyseinen testaus järjestelmä ei tunnu kovinkaan ketterältä . Androidin hyötyinä voi mainita työkalujen määrän. Yksinään testaukseen tarkoitettujen työkalujen niin ilmaisten kuin maksullisten, automaattisten ja manuaalisten määrä on valtaisa,joka osoittaa sen, että Android on käyttöliittymänä erittäin suosittu.

## Suositeltavat käytännöt

Ohjelmointi kielissä on aina perussääntönsä, joita on noudatettava niin sanottu syntaksi, jota seurata. Näiden syntaksien lisäksi on myös vaihtoehtoisia käytäntöjä, joita voi käyttää ohjelmoinnissa. Näitä methodeja kutsutaan suositeltaviksi käytännöiksi ja ne ovat enemmän ohjenuoria kuin sääntöjä. 

### Functional reactive programming

FRP:ssä on monia ohjenuoria, joita voi seurata johtuen kyseisen kielen laajuudesta sekä sen lukuisista moduleista. Tämän seurauksena yritän välttää tietyistä moduleista kertomista ja kerron kielen yleisistä suositeltavista käytännöistä.   (fsharpforfunandprofit)

* Tunnista tarvittavien tietojen tyypit sekä niiden toiminnat ja määrittele abstraktit luokat kyseisille tiedoille. 
* Tunnista tyypilliset toiminnot sekä laskenta mallit, jotka voidaan ilmaista korkeamman tason funktioina tai makroina.
* Älä käytä sulkeita kutsuessasi funktiota. 
* FRP: ssä välilyönti toimii erottimena funktion parametreille. FRP:ssä käytetään hyvin harvoin sulkeita. 

Esimerkki virheellisestä ja oikeasta ratkaisusta:

~~~~{.haskell}
let add x y = x + y
let result = add (1 2)  -- Väärin
    // error FS0003: This value is not a function and cannot be applied

let result = add 1 2    --Oikein 
~~~~

* Älä yritä syöttää kahta arvoa pilkun avulla yhteen parametriin. Kääntäjä tulkitsee arvot tupleksi ja palauttaa virheen. 
* Älä käytä liian monia argumentteja. Tämä aiheuttaa yleensä ”This value is not a function” – virheen. 
* Argumenttien määrä on erittäin tarkka käytettäessä printf – perheen funktioita. 
* Käytä puolipistettä luettelon erottimena:

Esimerkki Luettelon teosta FRP:ssä

~~~~{.haskell}
let list1 = [1,2,3]    -- Väärin -  Kyseinen lista olisi 3 – osainen tuple 
let list1 = [1;2;3]    -- Oikein – FRP:ssä toimiva lista.
~~~~

* Älä käytä huutomerkkiä NOT – operaattorina, sillä se palauttaa virheen. Functional reactive programming ottaa mallia SQL:sta koskien NOT – operaattoria sekä NOT EQUAL – operaattoria.

Esimerkki Oikea NOT – operaattori FRP:ssä:

~~~~{.haskell}
let y = true
let z = not y       --Oikein
~~~~

* Älä käytä yhtäsuuruusmerkkiä määrittelyssä. Määrittäessä arvon muuttujaan yhtäsuuruusmerkki antaa väärän tuloksen.

Esimerkki Oikeasta määrittelystä: 
~~~~{.haskell}
x <- x + 1         -- Oikein , Määrittää x+1   x:säksi. 
~~~~

Oheinen listaus koskien Functional reactive programming suositeltavia käytäntöjä on erittäin pintapuolinen, mutta yleishyödyllinen, kyseisten käytäntöjen toimiessa FRP – kielen eri moduleissakin.  
Seuraavassa osiossa tarkastelemme suositeltavia käytäntöjä Androidin näkökulmasta.

## Android

Androidin suositeltavat käytännöt niin kuin Functional reactive programming ovat erittäin runsaat ja laajat johtuen Androidin suosittavuudesta käyttöalustana. Eroavaisuutena Functional reactive programmingiin Androidissa on se, että sen suositeltavat käytännöt eivät ole jaettu eri moduleiden kesken.  
Seuraavaksi käymme lävitse tärkeitä alueita Android – ohjelmiston kehittämisessä. (developer.android.com )

* Suunnittele ja toteuta selkeä sekä käytännöllinen navigaatio. On tärkeätä luoda käyttöliittymä, jota käyttäjät pystyvät hyödyntämään ilman opastusta.
* Ilmoita käyttäjälle. On tärkeätä, että käyttäjällä on selkeä kokonaiskuva siitä miten ohjelma reakoi käyttäjän tekoihin. 
* Yritä luoda käyttöliittymä, jonka ominaisuudet ovat taaksepäin yhteensopivat.
* Syötteen antamisessa on suotavaa antaa käyttäjälle monia vaihtoehtoja. Androidissa on monia mahdollisuuksia koskien syötettä; näppäimistö, kosketusnäyttö sekä äänikomennot.
* Muista ottaa huomioon suunnittelemasi ohjelmiston suorituskyvyn, että ohjelma ei kuluta liikaa resursseja.
* Käytä mahdollisuutta ladata tietoja taustalla, jolloin käyttöliittymä ei hidastu. 
* Huomioi suunnittelemasi ohjelman turvallisuus verkkotapahtumissa.
* Huomioi myös käyttäjäsi. On erittäin tärkeätä huomioida ketä on ohjelman loppukäyttäjä, normaali kuluttuja tai yritys.

## Vertailu

Vertailtaessa Functional reactive programming ja Androidin suositeltavia käytäntöjä on erittäin ilmeistä, että kyseiset methodit eroavat toisistaan kuin kuu ja aurinko. Androidissa käytäntöjä ja materiaalia löytyy pilvin pimein kun taas Functional reactive programming materiaalin löytäminen tietystä modulista on erittäin haastavaa. Stackoverflown keskustelupalstalla mainittiin, että noin 6% ohjelmoijista käyttää FRP – kieltä ohjelmoinnissa. Otettuna huomioon kielen sisäiset modulit esim. Scala, reactive-banana niin yhden modulin käyttäjäkunta on varmaan 1% luokkaa kaikista ohjelmoijista. 
Androidissa suositeltavia käytöntäjä, jotka toimivat universaalisesti kyseisellä alustalla on erittäin paljon. FRP –  kielessä taas on niukemmin suositeltavia käytäntöjä, jotka toimisivat universaalisesti kyseisen kielen sisällä. 
Kyseinen universaalien käytäntöjen niukkuus on vain lievä laiminlyönti Functional reactive program - kielessä. Se ei tarkoita sitä, että kyseinen kieli olisi huono vaan, että kyseisen kielen hyödyt tulevat esille muilla osa-alueilla.  

## LÄHTEET

developer.android.com. http://developer.android.com/tools/testing/testing_android.html   Referred 12.03.2013.  

developer.android.com. http://developer.android.com/training/best-ux.html Referred 17.03.2013.

fsharpforfunandprofit. http://fsharpforfunandprofit.com/troubleshooting-fsharp/  Referred 14.03.2013

Hurme, J. 2011. THE BENEFITS OF USING UML- TOOLS IN EVALUATION AND TESTING OF ETM SOFTWARE. Opinnäytetyö. Tietojenkäsittelyn koulutusohjelma. Turku: Turun ammattikorkeakoulu, Salon yksikkö.

Mobiilikehitys.  http://mobiilikehitys.fi/testaus-androidilla/
Referred 12.03.2013. 

stackoverflow. http://stackoverflow.com/questions/4852251/is-there-a-software-engineering-methodology-for-functional-programming  Referred 11.03.2013.

