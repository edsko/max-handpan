default: watch-scaleselector

watch-trackselected:
	spago bundle-app --main TrackSelected --watch --clear-screen --to trackselected.js-purescript --then "./postprocess.sh trackselected.js"

watch-scaleselector:
	spago bundle-app --main ScaleSelector --watch --clear-screen --to scaleselector.js-purescript --then "./postprocess.sh scaleselector.js"

watch-frontend:
	spago bundle-app --main Frontend --watch --clear-screen --to frontend.js-purescript --then "./postprocess.sh frontend.js"

build-velocity:
	spago bundle-app --main Velocity --to velocity.js-purescript
	./postprocess.sh velocity.js

watch-velocity:
	spago bundle-app --main Velocity --watch --clear-screen --to velocity.js-purescript --then "./postprocess.sh velocity.js"

