from pyswip import Prolog


def generiere_szene():
    prolog = Prolog()
    rule_path = "prolog_test.pl"
    prolog.consult(rule_path)

    # # Basisfakten
    # prolog.assertz("pyramide(rot)")
    # prolog.assertz("pyramide(gruen)")
    # prolog.assertz("pyramide(blau)")
#
    # # Hilfsregeln
    # prolog.assertz("enthaelt(X, [X|_])")
    # prolog.assertz("enthaelt(X, [_|T]) :- enthaelt(X, T)")
    # prolog.assertz("laenge([], 0)")
    # prolog.assertz("laenge([_|T], L) :- laenge(T, L1), L is L1 + 1")
#
    # # Einschränkung der Länge der Strukturen
    # prolog.assertz("max_laenge(3)")
    # prolog.assertz("korrekt(Struktur) :- enthaelt(rot, Struktur), laenge(Struktur, L), max_laenge(Max), L =< Max")

    # Optimierte Query
    korrekte_szenen = []
    query, output = prolog.query("findall(Color, shape(pyramid, Color), Colors)"), "Colors"
    # query, output = prolog.query("findall((Type1, Type2), (shape(Type1, _), shape(Type2, _)), Pairs)"), "Pairs"
    # query, output = prolog.query("findall(Structure, correct(Structure), Structures)"), "Structures"

    for szene in query:
        korrekte_szenen.append(szene[output])

    return korrekte_szenen


# Ausführung
szenen = generiere_szene()
print("Generierte Szenen:", szenen)
