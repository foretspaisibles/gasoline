# Configuration of applications

Applications traditionnally gather configuration parameters from
several sources.  We provide a generic configuration facility
allowing a Gasoline host to easily import and blend configuration
values from different sources.

We discuss the use of the configuration facilities provided by
Gasoline by considering the case of the [punishment][src-punishment]
program a small utility printing 100 times the sentence “I must not
talk in class”.


## Concepts

- A **configuration parameter** is a parameter whose value is
  determined by the configuration system of Gasoline.  Its value can
  be queried by the Gasoline host.

- A **configuration source** is an external source which is analysed
  by Gasoline internal functions to determine the value of
  configuration parameters.  Examples of sources are command-line
  arguments, environment variables and configuration files.


## Creating configuration parameters

In the [punishment][src-punishment] program we work with the alias
`Application = Gasoline_Plain_Application` and we declared a
monolithic application component `comp` identified by the string
`"monolith"`.  The three configuration parameters `number`,
`paragraph` and `configfile` used by this application component are
declared by:

````ocaml
module Configuration =
struct
  open Application.Configuration

  let number =
    make_int comp ~flag:'n' ~env:"NUMBER"
    "number" 100
    "The number of times our punishment must be printed"

  let paragraph =
    make_string comp ~flag:'p' ~env:"PARAGRAPH"
    "paragraph" "I must not talk in class"
    "The paragraph we must copy as punishment"

  let configfile =
    make_string comp ~flag:'c' ~env:"CONFIGFILE"
      "configfile" ""
    "The name of an additional configuration file to read"
end
````

The functions `make_int` and `make_string` are taken from
`Application.Configuration`.  After these declarations, the value of
the configuration parameter `number` can be read with:

````ocaml
let n = Configuration.number ()
````

The user of this application can set the values of the
`Configuration.number` configuration parameter to 10 from the command
line using `-n 10`, from the environment by defining `NUMBER` to `10`
or from a configuration with the following contents:

````conf
[monolith]
number = 10
````

The configuration file must then be given to the `punishment` program,
using the the `-c` option or the `CONFIGFILE` environment variable.


## Configuration sources

Configuration sources define bindings between configuration parameters
and values for them. Configuration paramters are identified by a
dot-separated path whose first components are the path of the
application component owning the configuration parameter and the last
component the identifier of the configuration paramter.

The following external sources are used as configuration parameter
sources Gasoline:

- The command line;
- The environment;
- Configuration files;
- Hard-coded configuration alist.
- Hard-coded configuration here-document.

Configuration files exist in three forms, they might be *important*
and the application will exit if the file is not found, thay might be
optional and their absence is silently ignored, or they might be user
defined, referring to a special value given in the command-line or the
environment.

## Configuration cascade

These configuration sources can be combined with *merge* and
*override* semantics.  Thus an application can be writtent to allow
the system administrator to enforce some configuration parameters
settings on the application.

  [src-punishment]:		https://github.com/michipili/gasoline/blob/master/example/punishment/punishment.ml
