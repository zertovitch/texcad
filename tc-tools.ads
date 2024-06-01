package TC.Tools is

  type Detection is (empty_text, zero_sized_object, unknown_command, comment);

  type Occurrence is record
    number        : Natural;
    first_obj_pos : Positive;
  end record;

  type Detection_stat is array (Detection) of Occurrence;

  type Cleanup_action is array (Detection) of Boolean;
  no_cleanup_action : constant Cleanup_action := (others => False);

  procedure Detect (pic : Picture; stat : out Detection_stat);

  procedure Clean (pic : in out Picture; action : Cleanup_action);

end TC.Tools;
