# Gasoline

The Gasoline project aims at implementing a Unix-ish application
development framework for OCaml.  The framework will provide
application templates factoring application components bootstrapping,
configuration analyse and offering homogeneous diagnostic facilities.

It is written by Michael Grünewald and is distributed under the
[CeCILL-B][1] license agreement.

Users of Gasoline should be enabled to:

- Rapidly develop applications by using *application patterns* such as
  “Unix filter”, “tabular data processor” or “compiler”.
- Write large software suites whose elements offer homogeneous
  interfaces.
- Use standardised diagnostic facilities supporting
  internationalisation.
- Cleanly distinguish between application components and lower-level
  software engineering artifacts.
- Easily bootstrap and shutdown applications consisting of many
  modules.
- Use common file formats such as CSV or JSON in their applications.


## Current state

Gasoline is still beta-software, and many features or design elements
are susceptible to change before we reach version 1.0. Current
features are:

- A [diagnostic facility][2] similar to `printf` but better suited to
  internationalisation and having routing rules.
- [Configuration][3] based on files, environment variables and command
  line arguments.
- Configuration [cascading rules][4] which can be used to let system
  administrators enforce some settings of installed applications.
- Software [component management][5] which correctly bootstrap and
  shutdown the application, exception sent by the guest are caught and
  an emergency shutdown procedure is triggered.
- A [unit test][9] suite.

Applications can take advantage of a [simplified interface][6] to the
[Camomile library][7] and of writer functions, allowing to
produce simple SGML or HTML reports.

See the [ocamldoc generated documentation][8] of the `master` branch
for more details.


## Setup guide

The installation procedure is based on the portable build system
[bsdowl][10] based on BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][11]
   - OCaml
   - GNU Autoconf

2. Get the source, either by cloning the repository or by exploding a
   [distribution tarball](releases).

3. Optionally run `autoconf` to produce a configuration script. This
   is only required if the script is not already present.

4. Run `./configure`, you can choose the installation prefix with
   `--prefix`.

5. Run `make build`.

6. Optionally run `make test` to test your build.

7. Finally run `make install`.

Depending on how BSD Make is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.  The GNU
Make program usually give up the ghost, croaking `*** missing
separator. Stop.`

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Bonn, on October 21, 2014

   [1]: http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html
   [2]: http://github.com/michipili/gasoline/wiki/DiagnosticFacility
   [3]: http://github.com/michipili/gasoline/wiki/Configuration
   [4]: https://github.com/michipili/gasoline/wiki/Configuration#configuration-cascade
   [5]: http://github.com/michipili/gasoline/wiki/Component
   [6]: http://michipili.github.io/gasoline/reference/Unicode.html
   [7]: https://github.com/yoriyuki/Camomile
   [8]: http://michipili.github.io/gasoline/reference/index.html
   [9]: http://github.com/michipili/gasoline/wiki/UnitTesting
   [10]: https://github.com/michipili/bsdowl
   [11]: https://github.com/michipili/bsdowl/wiki/Install
