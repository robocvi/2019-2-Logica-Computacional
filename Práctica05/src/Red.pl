#Red Semantica, la imagen esta en el archivo diagramaRed.png
ejemplar(juan, persona).
ejemplar(luna, perro).
ejemplar(apolo, gato).

subclase(persona, vertebrado).
subclase(perro, vertebrado).
subclase(gato, vertebrado).
subclase(incisivo, diente).

tiene(persona, boca).
tiene(perro, boca).
tiene(gato, boca).
tiene(perro, collar).
tiene(gato, collar).
tiene(boca, incisivo).

puede(persona, correr).
puede(perro, correr).
puede(gato, correr).

relacionado(ejemplar, juan, persona).
relacionado(ejemplar, luna, perro).
relacionado(ejemplar, apolo, gato).
relacionado(subclase, persona, vertebrado).
relacionado(subclase, perro, vertebrado).
relacionado(subclase, gato, vertebrado).
relacionado(subclase, incisivo, diente).
relacionado(tiene, persona, boca).
relacionado(tiene, perro, boca).
relacionado(tiene, gato, boca).
relacionado(tiene, perro, collar).
relacionado(tiene, gato, collar).
relacionado(tiene, boca, incisivo).
relacionado(puede, persona, correr).
relacionado(puede, perro, correr).
relacionado(puede, gato, correr).

satisface(Prop, X, Y) :- relacionado(Prop, X, Y).
satisface(Prop, X, Y) :- relacionado(Prop, Z, Y), satisface(_, X, Z).

