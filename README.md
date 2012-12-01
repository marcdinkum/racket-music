This is a collection of functions for music data manipulation and
generating Lilypond files from Racket.

http://www.lilypond.org/

http://www.racket-lang.org/

Lilypond-functionaliteit
=======================

[1 december 2012]
Deze module is in ontwikkeling en kan nog veranderen.

Met de Lilypond module voor Racket kun je composities in Racket vertalen
naar input voor Lilypond. Deze composities voldoen aan het volgende:

* Noten worden numeriek en absoluut aangegeven, waarbij 60 middle-C is. In deze versie is 48 de laagste waarde die je kunt gebruiken
* De lengte van elke noot komt na de noot. Hierbij is 1 een hele noot, 2 een halve noot, 4 een kwartnoot etc.
* Een rust wordt aangegeve met "nap" (dutje)
* Een voice wordt aangegeven met "serial" en gelijktijdige elementen met "parallel"

Zie verder het voorbeeld onderaan.

Voorbereiding:

	1. Download lilypond.rkt van de github pagina
	https://raw.github.com/daanvanhasselt/racket_lilypond/master/lilypond.rkt

	2. Zet deze file de Racket-structuur
	Het handigste is om een eigen directory te hebben waar je je modules
	in zet. Bijvoorbeeld 'inf'. Deze maak je aan onder de directory 'collects'.

	<path-to-racket>/collects/inf

	Zet lilypond.rkt in deze directory.

	3. Inladen van de module
	In het definitions-window (het bovenste) van DrRacket zet je de volgende
	regel en voer dan met Run opnieuw uit:
	(require inf/lilypond)

	4. Voorbeeld.

	(define notes '(serial (note 60 4) (note 65 4) (nap 1) (parallel (note 60 4) (note 67 4))))

	(make-lilypond-file "test.ly" "Een liedje" "Ikke" "c" "major" notes)
