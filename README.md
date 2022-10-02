# Experiments in Dynamic Software, Visualisations, Creating a JVM Language, UI, and More

For detail about what this repository is about, see [this article](https://luisthiamnye.substack.com/p/adventures-in-dynamic-software-visualisations).

This is a soup of unpolished code from my explorations of Clojure's (and the JVM's) potential to create dynamic and humane computing experiences.

The projects in this repository are viewed in a graphical application, powered by Skija and JWM.

Usage: `clojure -M:dev` or `bb dev`

Important note: repository has been configured specifically for my personal computer (running Windows), so you may need to make some modifications to get some of the code working. Also, certain locally-installed fonts are assumed to be present.

Contents:

- Dependency graph viewer
- JVM language ("Squawk") that acts as a dynamic version of Java, as a Lisp dialect.
  - Text editor and navigator implemented in Squawk
- Error boundaries that show an image of the canvas before the error
- Stacktrace viewer where you can expand the frames to see the source of the relevant var and the line of the call site
  - Collapses repeating frames of a StackOverflowError
- Basic text input (broken)
- Basic vim-like editor -- to be done better
- Clojure var browser (editor is broken)
- Basic file browser with delete function
- Partially implemented structural Clojure editor (a bit broken on high-DPI displays)
- Glamorous Toolkit -like inspector (digger) -- use `tap>`
- Quantum circuit simulation
- Some partially implemented stuff for catching errors (see statusbar) and suspending/restarting threads via Java Debug Interface
- Visual display monitor brightness control (requires macOS and `ddcctl` command-line tool)
- Experiments developing new UI toolkits
- Text box with most features that you would expect
- Experiment with a more interesting file tree viewer

More info:

- https://luisthiamnye.substack.com/p/adventures-in-dynamic-software-visualisations
- https://twitter.com/LuisThiamNye/status/1509564956643057666
- https://twitter.com/LuisThiamNye/status/1509569953531322381
- https://www.youtube.com/watch?v=25ijoz5ZrB8

# License

Unless otherwise stated in a header, each file is distributed under Eclipse Public License 2.0 (see `LICENSE-EPL`).
The license described by `LICENSE-APACHE-2` applies to files that state they are licensed under Apache License 2.0.
