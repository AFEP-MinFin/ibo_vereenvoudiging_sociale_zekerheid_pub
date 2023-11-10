# IBO Vereenvoudiging Sociale Zekerheid
## Moeilijk Makkelijker Maken

*Deze repository bevat zowel openbare databestanden als notebooks met empirische analyses om de beleidsvorming van informatie te voorzien.*
*De bestanden zijn beschikbaar gemaakt door de directie Algemene Financiële & Economische Politiek (AFEP) van het Ministerie van Financiën.*
*De statistieken zijn ontwikkeld in het kader van het interdepartementale beleidsonderzoek naar de vereenvoudiging van de Sociale Zekerheid dat in 2022-2023 is uitgevoerd.*

De analyse bestaat uit twee onderdelen waarvan de documentatie beschikbaar is in de map `documenten`; dit zijn overigens ook bijlagen bij het IBO Moeilijk Makkelijker Maken. De stapelingsanalyse is uitgevoerd met microdata van het Centraal Bureau van de Statistiek (CBS). De achterliggende code en resulterende bestanden staan in de map `stapeling_analyse`. Daarnaast is een enquete afgenomen bij het LISS panel. De analyse hiervan staat in de map `liss_analyse`. De uitkomsten van de enquete zijn persoonsgegevens en niet beschikbaar op deze pagina. Indien u de resultaten wil repliceren kunt u deze opvragen bij CenterData. 

## Onderzoeksvraag
### Stapelingsanalyse
1. In hoeverre stapelen regelingen in de sociale zekerheid zich op bij personen?
2. Tussen welke regelingen in de sociale zekerheid wordt er veel gestapeld, ook in combinatie met toeslagen?

### LISS-enquête
1. Hoe verschilt ervaren complexiteit tussen regelingen in de sociale zekerheid?
2. Welk effect heeft de populatie van een regeling op de ervaren complexiteit?
3. Welk effect hebben voorwaarden uit het beleid op de ervaren complexiteit?

## Leeswijzer
### Stapelingsanalyse

- De code voor de stapelingsdataset staat onder het scripts [pipeline](stapeling_analyse/etl/pipeline.R).
- De main script voor de analyse van stapeling in de sociale zekerheid staat [hier](stapeling_analyse/main.R).
- De uitkomsten van de analyse kan gevonden worden verschillende Excel sheets onder `output`.
- Technische beschrijving van de analayse staat in het document [CBS Microdata Documentatie](documenten/CBS%20Microdata%20Documentatie.docx).

### LISS-enquête

- De main script voor de enquêteanalyse staat in [deze markdown](liss_analyse/documentatie_markdown.Rmd).
- De tabellen uit deze analyse staan onder `output`.
- De grafieken uit deze analyse staan onder `figure-latex`.
- Technische beschrijving van de analyse en interpretatie van de uitkomsten staan in het document [LISS Enquête-analyse](documenten/LISS%20Enquête-analyse.pdf).
- Additionele resultaten staan in de [Appendix](documenten/Appendix.pdf).

## Dankwoord

Dit onderzoek was niet mogelijk geweest zonder de gegevens van het Centraal Bureau voor de Statistiek en CenterData.
Daarbij bedanken wij de werkgroep en het secretariaat van het Interdepartmentaal Beleidsonderzoek Vereenvoudiging Sociale Zekerheid voor de expertise.
