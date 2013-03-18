## 8. Kehitystyökalut

Haskell on pitkälti täysin IDE:stä riippumaton kieli, eikä sen kirjoittamiseen vaadita erityisiä työkaluja. Laajalti käytössä ovat Vim, Emacs ja muut yleiset tekstieditorit. Useisiin yleisiin IDE:hin on olemassa tuki Haskellille (esimerkiksi Eclipseen ja KDevelopiin), ja tämän lisäksi joitakin täysin itsenäisiä ohjelmistoja on rakennettu (esimerkiksi Leksah).

Mitään graafisia editoreita käyttöliittymien rakentamiseen ei ole. Tämä johtuu pääosin käyttöliittymien suhteellisesta tuoreudesta Haskell-maailmassa.

Kuva 1. Kuvankaappaus Leksah-kehitysympäristöstä
![Kuvankaappaus Leksah:sta](images/leksah.png)

### 8.1 Cabal

Yleisesti Haskell rakentuu HackageDB:n kautta ladattaviin kehityskirjastoihin, jota varten on kehitetty Cabal-ohjelma. Cabal toimii samoin kuin useat muutkin paketinhallintaan rakennetut työkalut (esimerkiksi Linux-maailmassa apt ja portage), noutaen halutun kirjaston sekä sen vaatimat lisäkirjastot. Käyttöliittymiä rakentaessa Cabalin kautta voi noutaa oleelliset kirjastot (esim. reactive-banana-wx).

Esimerkiksi reactive-banana-wx asennus cabalin läpi voidaan suorittaa seuraavasti:

>cabal update

>cabal install wxdirect

>cabal install wxc

>cabal install wxcore

>cabal install wx

>cabal install reactive-banana-wx

## 8.2 Glasgow Haskell Compiler

Haskellin hiljakseltaan standardiksi muodostuva kääntäjä on Glasgow Haskell Compiler (GHC), joka tukee laajaa joukkoa eri alustoja ja arkkitehtuureja. GHC:n toiminta perustuu Haskellin kääntämiseen ensin C-kieleen, josta käännös jatkuu binääri-formaattiin. Yksinkertaisen .hs-tiedoston kääntäminen tapahtuu GHC:n avulla seuraavasti:

ghc --make hello.hs

Tästä seuraa alustan mukainen binääri, joka on oletuksena linkitetty dynaamisesti.
