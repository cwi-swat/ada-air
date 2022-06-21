pragma Ada_2012;
package body test is

    function fac(nb : Integer) return Integer
    is
        r : Integer := 0;
    begin
        for i in 1..nb loop
            r := r * i;
        end loop;
        if r < 0 then
            return 0;
        elsif r = 1 and r = 1 then
            return 1;
        else
            return r;
        end if;
    end fac; 

end test;