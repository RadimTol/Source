Pracovní aplikace pro monitoring médií a odborného tisku

Autor: Radim Tolasz (ještě pár let radim.tolasz@chmi.cz nebo trvalejší radim@chmi.cz).
Verze: duben 2026

Aplikace obsahuje R skript

- source.R prohledá vybrané média crossref podle klíčových slov a výsledky uloží do souborů source.csv a source-crossref.csv
- skript si hlídá duplikace a prochází jen aktuální den D a den D-1

Klíčová slova jsou v souboru keywords.txt

Aplikace obsahuje HTML soubory

- news.html obsahuje všechny publikace mimo crossref
- news-crossref.html má pouze odkazy do crossref

Pomocné soubory
- chmu_logo.svg - logo ČHMÚ do index.html
- last_run.txt a last_run_updat_date.txt - pomocnmé soubory pro kontrolu posledncíh běhů
