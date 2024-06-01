package TC.Input is

  procedure Load (pic       : in out Picture;
                  macro     :        Boolean;
                  file_name :        String);

  Load_error : exception;

end TC.Input;
