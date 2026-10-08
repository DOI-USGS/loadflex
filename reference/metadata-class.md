# Store metadata relevant to a load model.

`metadata` objects store metadata for a load model of any type (e.g.,
[`loadModel`](http://doi-usgs.github.io/loadflex/reference/loadModel.md),
[`loadInterp`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md),
[`loadReg2`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md),
or
[`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)).
See an example object at
`data(`[`eg_metadata`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)`)`.
Metadata can be created and modified by calls to
[`metadata`](http://doi-usgs.github.io/loadflex/reference/metadata.md)
or
[`updateMetadata`](http://doi-usgs.github.io/loadflex/reference/metadata.md).
Values and relevant information can be accessed by
[`getCol`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md),
[`getUnits`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md),
or
[`getInfo`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md).

## Slots

- `constituent`:

  character. The name of the data.frame column describing the
  concentration of the solute or material to be modeled.

- `consti.name`:

  character. Consituent long name, for use in plots and reports

- `flow`:

  character. The name of the data.frame column describing flow
  (discharge)

- `load.rate`:

  character. The name of the data.frame column, if it exists, describing
  instantaneous load

- `dates`:

  character. The name of the data.frame column describing date and/or
  time

- `flow.units`:

  character. The units in which flow is both input and exported.

- `conc.units`:

  character. The units in which concentration is both input and
  exported.

- `load.units`:

  character. The units in which load (flux) is both input and exported.

- `load.rate.units`:

  character. The units in which load (flux) is reported by
  predict.loadModel().

- `station`:

  deprecated. character. A description of the sampling station or site.
  It's now recommended to use `site.name` and/or `site.id` instead.

- `site.name`:

  character. A description of the station or site where the
  concentration of the constituent was measured. This slot or `site.id`
  is a better place for a site identifier than the `station` slot, which
  is deprecated.

- `site.id`:

  character Station ID for the site where the concentration of the
  constituent was measured.

- `lat`:

  numeric Station latitude where concentration (and possibly also
  discharge) was measured.

- `lon`:

  numeric Station longitude where concentration (and possibly also
  discharge) was measured.

- `basin.area`:

  numeric Area of the drainage basin contributing water to the site
  where concentrations were measured.

- `flow.site.name`:

  character Long name of the station where flow was monitored, if
  different from where concentration was monitored (as in `site.name`).

- `flow.site.id`:

  character Station ID of the station where flow was monitored, if
  different from where concentration was monitored (as in `site.id`).

- `flow.lat`:

  numeric Latitude of the station where flow was monitored, if different
  from where concentration was monitored (as in `site.lat`).

- `flow.lon`:

  numeric Longitude of the station where flow was monitored, if
  different from where concentration was monitored (as in `site.lon`).

- `flow.basin.area`:

  numeric Area of the drainage basin contributing water to the site
  where flow was monitored, if different from where concentration was
  monitored (as in `basin.area`).

- `basin.area.units`:

  character Units of the values in `basin.area` and `flow.basin.area`
  (must be the same for both). Basin areas are used to compute yields as
  loads per contributing (basin) area.

- `custom`:

  ANY. Empty by default, but may be modified to store any additional
  data the user wants to track.
