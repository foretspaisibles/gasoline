# Gasoline

The Gasoline project aims at implementing a Unix-ish application
development framework for OCaml.  The framework provides application
templates factoring application components bootstrapping,
configuration analyse and common application patterns, such as
[filter][doc-filter], [secure tool][doc-secure-tool] or
[dæmon][doc-daemon].

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


## Current state

Gasoline is still beta-software, and many features or design elements
are susceptible to change before we reach version 1.0. Current
features are:

- [Configuration](./doc/Configuration.md) based on files, environment
  variables and command line arguments.
- Configuration cascading rules which can be used to let system
  administrators enforce some settings of installed applications.
- Software [component management](./doc/Component.md)
  which correctly bootstraps and
  shutdowns the application, exception sent by the guest are caught and
  an emergency shutdown procedure is triggered.
- A [diagnostic facility][doc-message] similar to `printf` but better suited to
  internationalisation and having routing rules.

Applications can take advantage of a
[simplified interface][doc-unicode] to the
[Camomile library][camomile-home] and of writer functions, allowing to
produce simple SGML or HTML reports.

See the [ocamldoc generated documentation][doc-index] of the `master`
branch for more details.


## Free software

It is written by Michael Grünewald and is distributed as a free
software: copying it  and redistributing it is
very much welcome under conditions of the [CeCILL-B][licence-url]
licence agreement, found in the [COPYING][licence-en] and
[COPYING-FR][licence-fr] files of the distribution.


## Setup guide

It is easy to install **Gasoline** using **opam** and its *pinning*
feature.  In a shell visiting the repository, say

```console
% opam pin add gasoline .
```

It is also possible to install **Gasoline** manually.
The installation procedure is based on the portable build system
[BSD Owl Scripts][bsdowl-home] written for BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][bsdowl-install]
   - OCaml
   - [broken][broken-home]
   - [camomile][camomile-home]
   - [configuration][configuration-home]
   - [getopts][getopts-home]
   - [lemonade][lemonade-home]
   - [mixture][mixture-home]
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

Depending on how **BSD Make** is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.
The **GNU Make** program usually give up the ghost, croaking
`*** missing separator. Stop.` when you mistakingly use it instead of
**BSD Make**.

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Bonn, on October 21, 2014

  [broken-home]:        https://github.com/michipili/broken
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
  [camomile-home]:      https://github.com/yoriyuki/Camomile
  [configuration-home]: https://github.com/michipili/configuration
  [doc-daemon]:         http://michipili.github.io/gasoline/master/Gasoline_Plain_Daemon.html
  [doc-filter]:         http://michipili.github.io/gasoline/master/Gasoline_Plain_Filter.html
  [doc-index]:          http://michipili.github.io/gasoline/master/index.html
  [doc-message]:        http://michipili.github.io/gasoline/master/Gasoline_Generic_Message.html
  [doc-secure-tool]:    http://michipili.github.io/gasoline/master/Gasoline_Plain_SecureTool.html
  [doc-unicode]:        http://michipili.github.io/gasoline/master/Gasoline_Unicode.html
  [getopts-home]:       https://github.com/michipili/getopts
  [lemonade-home]:      https://github.com/michipili/lemonade
  [licence-en]:         COPYING
  [licence-fr]:         COPYING-FR
  [licence-url]:        http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html
  [mixture-home]:       https://github.com/michipili/mixture
