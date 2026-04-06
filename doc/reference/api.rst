TOML Fortran API
================

The TOML Fortran API is available via the main :f:mod:`tomlf` module.
Additionally, the :f:mod:`tomlf_all` module provides access to all public entities in the library for convenience.
However, it is recommended to use the main :f:mod:`tomlf` module, if other entities should be made available there, please open an issue or a pull request.
Finally, the :f:mod:`tomlf_version` module provides access to the version information of the library.
Note that there are two ways to ways to access the version information, either from the module constants at compile time or the :f:subr:`get_tomlf_version` subroutine at runtime.

.. f:automodule:: tomlf
.. f:automodule:: tomlf_all
.. f:automodule:: tomlf_version