# Application components

Gasoline provides facilities to manage application components. An
**application component** is the largest hierarchical unit of
organisation in a software package, apart from the software package
itself.


## Application components as engineering artifacts

From the perspective of software engineering, application components
as understood by Gasoline have the following characteristcs or
responsabilities.

- Application components are the largest hierarchical unit of
  organisation in a software package, apart from the software package
  itself.

- Application components manage their errors.  Exceptions are not
  allowed to flow outside of functions defined as the interface of a
  software components.

- Application components are responsible for the report of diagnostic
  messages.  As a special case, internationalisation of diagnostic
  messages is realised at the level of application components.
  Hierarchical units of lower importance should not issue diagnostic
  messages themselves, instead they should allow dependency injection
  to let the client component handle diagnostic itself.

- Application components are allowed to require termination of the
  application on error conditions.  Hierarchical units of lower
  importance should not require termination of the application but
  report a fatal error condition, for instance.

- Application components should be used to write loosely coupled
  application.  The application logic should be expressed in terms of
  components and of the abstractions they provide, without accessing
  lower-level abstractions.


## Application components implemented in Gasoline

An application component is defined and registered as follows — the
example assumes `Application = Gasoline_Plain_Application` and is
taken from [wordgen][src-wordgen]:

````ocaml
let comp = {
  Application.Component.make
    ~name:"automaton"
    ~description:"Automaton parameters"
    ~getopt_prefix:'A'
  ()
```

Let us review the fields of the component definition, most of them
bing optional:

- **name** is the name of the component.
- **version** is a version, it might be used to precisely describe an
  application in a bug report, for instance.
- **require** is the list of modules on which the defined module depends.
- **provide** is a list of facility provided by the defined module.
- **description** is a one line description of the module.
- **configuration prefix** is a configuration path used to qualify
  configuration paramters.
- **getopt_prefix** is a letter introducing long-option style command line
  paramters.
- **bootstrap** is a hook to run when the component is initialised.
- **shutdown** is a hook to run when the component is brought down.


## Bootstrap, shutdown and emergency shutdown

The bootstrapping procedure of the application calls all the
bootstrapping callbacks of registered modules in dependency
order. That is:

- If a module `a` requires a module `b`, it is guaranteed that the
  bootstrapping callback of `b` will be called before the
  bootstrapping callback of `a`.

- If a module `a` is provided my modules `b1` … `bn`, it is guaranteed
  that the bootstrapping callbacks of `b1` … `bn` will be called before
  the bootstrapping callback of `a`.

- If one of these requirements cannot be fulfilled — that is, in case
  of circular dependency — no callback is called and the application
  terminates.

The shutdown procedure of the application calls all the the shutdown
callback of registered modules in the reverse order.

If one of the callbacks fails, an emergency shutdown takes place.

  [src-wordgen]:		https://github.com/michipili/gasoline/blob/master/example/wordgen/wordgen.ml
