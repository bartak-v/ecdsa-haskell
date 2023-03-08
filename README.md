# Readme

Projekt byl testován na serveru Merlin (GHC 7.6.3) a vyvíjen na GHC 9.2.5 - je kompatibilní s oběma verzemi.

# Splnění zadání

Program implementuje všechny přepínače ze zadání (-i, -k, -s, -h). Dokáže číst ze STDIN i ze souboru.

# Věci navíc

## Křivky navíc

Program byl testován na některých běžně používaných křivkách typu y^2≡x^3+ax+b, ostatní typy nebyly testovány. Příklady těchto křivek lze nalézt ve složce 'curves/'.

## Parsování

Program dokáže parsovat i "špatně" naformátovaný a zašuměný vstupní soubor (např. 'test/Information_test_misformatted.in'). Vstupní soubor ovšem musí obsahovat všechny klíčová slova (parametry) pro daný přepínač. Parser bere vše, co není parametr Eliptické Křivky (["p:", "a:", "b:", "x:", "y:", "n:", "h:", "d:", "Q:", "Hash:", "r:", "s:"]) jako komentáře a ignoruje takové řádky. Díky tomu mohou být parametry zadány napřeskáčku a prokládány komentáři. Parametry na vstupu taktéž nesmí být duplicitní.

Díky tomu lze i vstupní formát zjedodušit (např. soubor 'curves/secp256k1' - stejně to platí i pro ostatní přepínače).

Každý EC parametr však musí být na vlastním řádku a nesmí končit komentářem. Parametry mohou být zadány jako hexadecimální čísla typu (0xXXXX...) nebo jako kladná čísla v desítkové soustavě (123). Toto neplatí pro veřejný klíč, ten je ukládán a parsován ve formátu: `0x04<PADDING>PUB_KEY_X<PADDING>PUB_KEY_Y`, kde souřadnice jsou čísla v hexadecimální soustavě. Parser je case-sensitive co se týče keywords, ale hexadecimální čísla mohou být velkými i malými písmy.

## Ověřování

Program dokáže dělat podpisy nad libovolným hashovacím algoritmem (hash je programem zkrácen na délku prvočísla n). Hash by měl být delší nebo stejně dlouhý jako prvočíslo n.

## Testování

K programu jsem přidělal dva bash skripty, které dokážou jeho funkčnost otestovat. Pro spuštění testů pro všechny křivky z 'curves/' lze zavolat ./test_suite.sh.

* test_suite.sh - postupně prochází všechny příkladové křivky ze složky 'curves/' a volá pro ně test.sh.
* test.sh - má dva vstupní parametry, soubor s parametry křivky a délku hashe (./test.sh curves/secp384r1 384). Tento testovací skript postupně spouští všechny přepínače (módy) ECDSA programu a předává mezi nimi výstupy. V rámci běhu se vygeneruje náhodný Hash dané délky a tento je podepsán a podpis následně zkontrolován pomocí ECDSA programu. Test je úspěšný, pokud je podpis validní. Každý běh test.sh obsahuje nově generované klíče a podpisy. V případě, že kterákoliv část programu selže je test nevalidní.

Případně se dá testovat program s parametry ze zadání projektu (ty se nachází ve složce test/).

### Reference

* Implementováno dle <https://secg.org/sec1-v2.pdf>
* <https://neuromancer.sk/std/secg>
* <http://learnyouahaskell.com>
* <https://www.cs.miami.edu/home/burt/learning/Csc609.142/ecdsa-cert.pdf>
* <https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm>
* <https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm>