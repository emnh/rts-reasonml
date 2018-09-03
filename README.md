# Prerequisites

```bash
sudo apt install ocaml
sudo npm install -g https://github.com/reasonml/reason-cli/archive/3.0.4-bin-linux.tar.gz
sudo npm install -g concurrently
sudo npm install -g budo
npm install bs-platform
npm install
git submodule update --init
npm start
```

Go to http://localhost:8080/index .

# Resources
 - https://opengameart.org/content/roguelike-tiles-large-collection
 - https://www.npmjs.com/package/hqx

# TODO
 - Perf fixes: uniform array concat, currying, caml_get_public_method
 - Multi-view from different angles
 - Fix reuse of WebGL context on reload
 - Tower defense game
 - Camera movement with WASD/arrows, mouse trackball rotation and mouse scrolling, FPS mode
 - Simple 3D boats or cars carrying 2D units
 - Tower placement with mouse
 - Unit selection
 - Finding k nearest neighbours
 - Attack animation
 - Damage graphs and heat maps
 - Health bars
 - Unit deaths or explosions
 - Fish jumping in sea, underwater view
 - Volcanoes, floods, smoke and forest fires
