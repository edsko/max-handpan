watch-frontend:
	spago bundle-app --main Frontend --watch --clear-screen --to frontend.js-purescript --then "./postprocess.sh frontend.js"

build-velocity:
	spago bundle-app --main Velocity --to velocity.js-purescript
	./postprocess.sh velocity.js

watch-velocity:
	spago bundle-app --main Velocity --watch --clear-screen --to velocity.js-purescript --then "./postprocess.sh velocity.js"

