with Office_Applications;

package TC.GWin.Options is

  --  * Loads and Saves options for the CURRENT_USER :

  procedure Load (mru : out Office_Applications.MRU_Info);
  procedure Save (mru : in Office_Applications.MRU_Info);

  --  * Clears options for ALL USERS :

  procedure Clear;
  Clear_failed : exception;

end TC.GWin.Options;
