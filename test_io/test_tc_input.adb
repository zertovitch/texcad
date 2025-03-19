with TC.Input;

procedure Test_TC_Input is
  procedure Test (name : String) is
    pic: TC.Picture;
  begin
    TC.Input.Load (pic, False, name & ".tcp");
  end Test;

begin
  Test ("test_io/test_makebox");
end Test_TC_Input;
