# My elm-pages basic setup

Basic adaptation for my web-pages

## Para actualizar desde remote/mi-adapt
1. Establecemos el remote en _Magit_ con `Ma` y establecemos ahí
el nombre "epa" y definimos el remote epa como:  `github.com/rolojf/elm-pages-adapt.git`.

2. Confirmar los remotes en la terminal con `git remote -v`


3. Obtener los cambios usando este comando desde la terminal:
`$git merge --no-edit epa/main --allow-unrelated-histories`
Y continuar los cambios resolviendo conflictos desde magit.


