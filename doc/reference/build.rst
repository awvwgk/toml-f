The build interface
===================

The TOML Fortran interface for working with TOML data structures is composed out of two main interfaces, the ``get_value`` interface for accessing TOML values and the ``set_value`` interface for creating values.
Those two interfaces are avaialble in the :f:mod:`tomlf_build` module and reexported in the main :f:mod:`tomlf` module for convenience.
To access nested values, the :f:type:`toml_path` type provides a convenient way to access values in a nested TOML document.
Finally, to handle the merging of TOML documents, the :f:subr:`merge_table` and :f:subr:`merge_array` subroutines are available.

.. f:automodule:: tomlf_build
.. f:automodule:: tomlf_build_array
.. f:automodule:: tomlf_build_keyval
.. f:automodule:: tomlf_build_table

.. f:automodule:: tomlf_build_merge

.. f:automodule:: tomlf_build_path
