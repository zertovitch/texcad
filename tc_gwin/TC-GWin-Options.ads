package TC.GWin.Options is

  -- * Loads and Saves options for the CURRENT_USER :

  procedure Load;
  procedure Save;

  -- * Clears options for ALL USERS :

  procedure Clear;
  Clear_failed: exception;

end TC.GWin.Options;