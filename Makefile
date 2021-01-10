script=dist/main.js

.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i

.PHONY: client
client:
	cd client && elm make src/Main.elm --optimize --output=$(script)

.PHONY: minify
minifi: client
	uglifyjs client/$(script) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output client/$(script)

.PHONY: install
install: client
	stack install --flag fundeps:WithJS

.PHONY: test
test:
	stack test

.PHONY: elm-live
elm-live:
	cd client && elm-live src/Main.elm -- --output=$(script)