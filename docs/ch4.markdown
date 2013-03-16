- Esittele UI-komponentit ryhmittäin, anna pieni kuvaus kustakin, mielellään myös ulkoasu
- Miten komponenttia sovelletaan, perimällä?
- Anna pieni esimerkki kummastakin
- Vertaile X:ää ja Y:tä tässä mielessä.

## UI-komponentit tekniikoittain

Toivon, että tehtävän tarkoituksena ei ole listata _kaikkia_ komponentteja,
sillä niitä on kummassakin järjestelmässä erittäin paljon. Sen sijaan listaan
kategoriat ja joitain esimerkkejä kustakin.

Omien komponenttien tekeminen on samanlaista kummassakin järjestelmässä, mutta
Kummassakin pitäisi olla tarpeeksi valmiita komponentteja kaikkiin tarpeisiin.
Jos komponentit ei riitä, voidaan tehdä aliluokkia valmiista ohjaimista.

Esimerkki [Arithmetic.hs][counter] näyttää miten WxWidgets kirjastoa voidaan
käyttää Haskellilla. Siinä luodaan kolme ohjainta, kaksi tekstikenttää ja yksi
ulostulo.

~~~~{.haskell}
input1 <- entry f []
input2 <- entry f []
output <- staticText f []

set f [layout := margin 10 $row 10 $
        [widget input1, label "+", widget input2,
        label "=", minsize (sz 40 20) $ widget output]
~~~~

Esimerkki [Button][button-xml] näyttää miten Android-järjestelmässä voidaan
luoda perinteinen painike.

~~~~{.xml}
<Button
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:text="@string/button_text"
    ... />
~~~~

### WxWidgets

#### Hallitut ikkunat

- `wxWizard`
    - 'Velhot' ovat dialogeja jotka koostuvat useammasta peräkkäisestä
      dialogista.

#### Sekalaiset ikkunat

- `wxPanel`
    - Ohjain johon muita ohjaimia voidaan asettaa. Yhtä monimutkainen kuin
      dialogi, mutta mikä tahansa ikkuna voi olla isäntänä.

#### Yleiset dialogit

- `wxFileDialog`
    - Perinteinen tiedostonvalitsin.

#### Ohjaimet

- `wxButton`
    - Perinteinen painike. Voi sisältää tekstiä.
- `wxChoice`
    - Valintaohjain jonka avulla voidaan valita yksi.
- `wxSlider`
    - Ohjain jonka avulla voidaan muokata arvoa siirtämällä valintaa.

#### Sekalaiset valitsimet

- `wxFontPickerCtrl`
    - Ohjaimen avulla voidaan valita fontti

#### Menut

- `wxMenu`
    - Näyttää listan menuja

#### Ulkoasuohjaimet

- `wxGridSizer`
    - Laittaa ohjaimet ristikkoon

### Android

#### Ohjaimet

- `Button`
    - Painike joka voi sisältää tekstiä
- `EditText`
    - Editoitava tekstikenttä
- `CheckBox`
    - Valintapainike. Ohjaimen avulla voidaan valita useampi elementti
      isommasta joukosta
- `RadioButton`
    - Valintapainike. Ohjaimen avulla voidaan valita yksi elementti elementtien
      joukosta
- `ToggleButton`
    - Painike jolla on kaksi tilaa.

#### Menut

- menu
    - Varasto menuille

#### Toimintapalkki

- `ActionBar`
    - Navigointiohjain. Auttaa käyttäjää navigoimaan sovelluksessa

#### Asetukset

- `Preference`
    - Yleiset ohjeet ja käytännöt asetusten tekemiselle. Ei varsinaisesti
      yksittäinen ohjain

#### Dialogit

- `AlertDialog`
    - Varoitusdialogi. Voi sisältää otsikon ja maksimissaan kolme painiketta

#### Notifikaatiot

- `Notification.Builder`
    - Ei varsinaisesti ohjain, mutta määrittelee notifikaation. Notifikaatio on
      merkintä, mikä tulee androidin ylävalikkoon


[counter]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana-wx/src/Arithmetic.hs Arithmetic.hs
[button-xml]: https://developer.android.com/guide/topics/ui/controls/button.html
