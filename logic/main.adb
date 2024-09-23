with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

procedure Main is

  function Is_Valid_Var (s: Character) return Boolean is
    --Valid_Range1 : constant Character_Range := ('A','Z');
    --Valid_Range2 : constant Character_Range := ('a','z');
  begin
    return s in letter_lowercase;
  end Is_Valid_Var;

  Arg : Character := 'a';
  valid : Boolean := false;
begin
  for Next in 1 .. Ada.Command_Line.Argument_Count loop
    declare 
      ArgS : String := Ada.Command_Line.Argument (Next);
    begin
      if ArgS'Length /= 1 then
        Ada.Text_IO.Put_Line ("Bad Arg!");
      end if;
      Arg := ArgS(1);
      valid := Is_Valid_Var (arg);
      Ada.Text_IO.Put_Line (arg'Image);
      Ada.Text_IO.Put_Line (valid'Image);
    end;
  end loop;
end Main;
