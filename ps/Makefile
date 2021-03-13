default: watch-retuner

all:
	spago bundle-app --main TrackSelected --to trackselected.js-purescript --then "./postprocess.sh trackselected.js"
	spago bundle-app --main ScaleSelector --to scaleselector.js-purescript --then "./postprocess.sh scaleselector.js"
	spago bundle-app --main Frontend --to frontend.js-purescript --then "./postprocess.sh frontend.js"
	spago bundle-app --main Velocity --to velocity.js-purescript --then "./postprocess.sh velocity.js"
	spago bundle-app --main Retuner --to retuner.js-purescript --then "./postprocess.sh retuner.js"

watch-retuner:
	spago bundle-app --main Retuner --watch --clear-screen --to retuner.js-purescript --then "./postprocess.sh retuner.js"

watch-trackselected:
	spago bundle-app --main TrackSelected --watch --clear-screen --to trackselected.js-purescript --then "./postprocess.sh trackselected.js"

watch-scaleselector:
	spago bundle-app --main ScaleSelector --watch --clear-screen --to scaleselector.js-purescript --then "./postprocess.sh scaleselector.js"

watch-frontend:
	spago bundle-app --main Frontend --watch --clear-screen --to frontend.js-purescript --then "./postprocess.sh frontend.js"

watch-velocity:
	spago bundle-app --main Velocity --watch --clear-screen --to velocity.js-purescript --then "./postprocess.sh velocity.js"

