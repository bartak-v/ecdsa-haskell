# Readme

Projekt byl testován na serveru Merlin (GHC 7.6.3) a vyvíjen na GHC 9.2.5 - je kompatibilní s oběma verzemi.

# Splněné zadání

Program implementuje všechny přepínače ze zadání (-i, -k, -s, -h). Dokáže číst ze STDIN i ze souboru.

# Věci navíc

## Parsování

Program dokáže parsovat i "špatně" naformátovaný a zašuměný vstupní soubor (např. 'test/Information_test_misformatted.in'). Vstupní soubor ovšem musí obsahovat všechny klíčová slova (parametry) pro daný přepínač. Parser bere vše, co není parametr Eliptické Křivky (["p:", "a:", "b:", "x:", "y:", "n:", "h:", "d:", "Q:", "Hash:", "r:", "s:"]) jako komentáře a ignoruje takové řádky. Díky tomu mohou být parametry zadány napřeskáčku a prokládány komentáři.

Každý parametr však musí být na vlastním řádku a nesmí končit komentářem. Parametry mohou být zadány jako hexadecimální čísla typu "0xXXXX..." nebo jako čísla v desítkové soustavě (123).

## Testování

## Křivky navíc

Program byl testován na běžně používaných křivkách typu y^2≡x^3+ax+b, ostatní typy nebyly testovány.
