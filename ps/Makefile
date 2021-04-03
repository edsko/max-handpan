default: watch-retuner

all:
	spago bundle-app --main TrackSelected --to temp/trackselected.js-purescript --then "./postprocess.sh temp/trackselected.js"
	spago bundle-app --main ScaleSelector --to temp/scaleselector.js-purescript --then "./postprocess.sh temp/scaleselector.js"
	spago bundle-app --main Frontend --to temp/frontend.js-purescript --then "./postprocess.sh temp/frontend.js"
	spago bundle-app --main Velocity --to temp/velocity.js-purescript --then "./postprocess.sh temp/velocity.js"
	spago bundle-app --main Retuner --to temp/retuner.js-purescript --then "./postprocess.sh temp/retuner.js"

watch-retuner:
	spago bundle-app --main Retuner --watch --clear-screen --to temp/retuner.js-purescript --then "./postprocess.sh temp/retuner.js"

watch-trackselected:
	spago bundle-app --main TrackSelected --watch --clear-screen --to temp/trackselected.js-purescript --then "./postprocess.sh temp/trackselected.js"

watch-scaleselector:
	spago bundle-app --main ScaleSelector --watch --clear-screen --to temp/scaleselector.js-purescript --then "./postprocess.sh temp/scaleselector.js"

watch-frontend:
	spago bundle-app --main Frontend --watch --clear-screen --to temp/frontend.js-purescript --then "./postprocess.sh temp/frontend.js"

watch-velocity:
	spago bundle-app --main Velocity --watch --clear-screen --to temp/velocity.js-purescript --then "./postprocess.sh temp/velocity.js"
