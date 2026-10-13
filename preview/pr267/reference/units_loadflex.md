# Units-related datasets for the loadflex package

These datasets are provided for managing units within the loadflex
package. It is recommended NOT to use these datasets directly within
your code, as they may change structure or contents in the future.

## Datasets

- `valid.metadata.units`:

  A data.frame with character columns "unit", "standard", and
  "dimension". Used (1) to determine whether units in slots of a
  \`metadata\` object have the appropriate dimensionality; e.g.,
  `conc.units` must have dimensions of mass or count per volume, and (2)
  to convert among units of the same dimensionality.

- `freeform.unit.translations`:

  A data.frame with character columns "new" and "old". Used for mapping
  freeform units ("old") to the specific format required by loadflex
  ("new").

- `unit.conversions`:

  A data.frame with character columns "numerator", "denominator", and
  numeric column "value", one conversion per row. Used for converting
  among
