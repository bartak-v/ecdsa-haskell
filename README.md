# Readme

Projekt byl testován na serveru Merlin (GHC 7.6.3) a vyvíjen na GHC 9.2.5 - je kompatibilní s oběma verzemi.

## Splnění zadání

Program implementuje všechny přepínače ze zadání `(-i, -k, -s, -v)` a `--help`. Dokáže číst ze `STDIN` i ze souboru.

## Věci navíc

### Křivky navíc

Program byl testován na některých běžně používaných křivkách s definiční rovnicí `y^2≡x^3+ax+b`, ostatní typy nebyly testovány. Příklady těchto křivek lze nalézt ve složce `curves/`, se všemi program korektně funguje.

### Parsování

Vyhnul jsem se používání `parsec`, tudíž jsem si všechny parsery psal sám - což byla zpětně asi chyba, ale i přesto je parser robustní a snese (do určité míry) špatně formátovaný vstup (např. `test/Information_test_misformatted.in`). Díky tomu lze i vstupní formát zjedodušit (např. soubor `curves/secp256k1`).

Vstupní soubor ovšem musí obsahovat všechny klíčová slova (parametry) důležitá pro daný přepínač. Parser bere vše, co není standardní parametr Eliptické Křivky (`["p:", "a:", "b:", "x:", "y:", "n:", "h:", "d:", "Q:", "Hash:", "r:", "s:"]`) jako komentáře a ignoruje takové řádky. Díky tomu mohou být parametry zadány napřeskáčku a prokládány komentáři. Parametry na vstupu taktéž nesmí být duplicitní.

Každý EC parametr však musí být na vlastním řádku a nesmí končit komentářem - tzn. na řádku musí být čistě `p: 0x0000` né `p: 0x0000#KOMENTÁŘ`. Parametry mohou být zadány jako hexadecimální čísla typu (`0xFFFF...`) nebo jako kladná čísla v desítkové soustavě (`123`). Toto neplatí pro veřejný klíč, ten je ukládán a parsován ve formátu: `0x04<PADDING>PUB_KEY_X<PADDING>PUB_KEY_Y`, kde souřadnice jsou čísla v hexadecimální soustavě. Parser je case-sensitive co se týče keywords, ale hexadecimální čísla mohou být velkými i malými písmy.

### Ověřování

Program dokáže dělat podpisy nad libovolným hashovacím algoritmem (`hash` je programem zkrácen na délku prvočísla `n`). Hash by měl být delší nebo stejně dlouhý jako prvočíslo `n`.

### Testování

K programu jsem přidělal dva bash skripty, které dokážou jeho funkčnost otestovat. Pro spuštění testů pro všechny křivky z `curves/` lze zavolat `make test` nebo `./test_suite.sh`.

* `test_suite.sh` - postupně prochází všechny příkladové křivky ze složky `curves/` a volá pro ně `test.sh`.
* `test.sh` - má dva vstupní parametry, soubor s parametry křivky a délku hashe (`./test.sh curves/secp384r1 384`). Tento testovací skript postupně spouští všechny přepínače (módy) ECDSA programu a předává mezi nimi výstupy. V rámci běhu se vygeneruje náhodný Hash dané délky (pomocí OpenSSL). Tento Hash je podepsán a podpis následně zkontrolován pomocí ECDSA programu. Test je úspěšný, pokud je podpis validní. Každý běh `test.sh` obsahuje nově generované klíče a podpisy. V případě, že kterákoliv část programu selže, je test nevalidní.

Samozřejmě se dá testovat program s parametry ze zadání projektu (ty se nachází ve složce `test/`).

## Reference

* Implementováno dle <https://secg.org/sec1-v2.pdf>
* <https://neuromancer.sk/std/secg>
* <http://learnyouahaskell.com>
* <https://www.cs.miami.edu/home/burt/learning/Csc609.142/ecdsa-cert.pdf>
* <https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm>
* <https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm>