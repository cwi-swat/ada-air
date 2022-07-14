@license{
Copyright (c) 2022, TNO (ESI) and NWO-I Centrum Wiskunde & Informatica (CWI)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}
@author{Jurgen J. Vinju - Centrum Wiskunde & Informatica}
@author{Damien De Campos - TNO ESI}
@author{Pierre van de Laar - TNO ESI}
module lang::ada::AST
extend analysis::m3::AST;

alias Ada_Node = node;
alias Maybe[&T] = list[&T];

data Entry_Point = Compilation_Units_Kind (list[Compilation_Unit] As_Compilation_Units)
| Statements_Kind (list[Statement] As_Statements);

% for t in RascalContext.chained_constructors:
data ${t.get_name()}(loc src=|unknown:///|) =\
<%
vertical_bar = " "
%>\
        % for c in t.get_constructors():
${vertical_bar}${c.get_name()}(\
<%
vertical_bar = "| "
comma = ""
%>\
                % for field_name, field_type in c.get_fields().items():
${comma}${field_type} ${field_name}\
<%
comma = ", "
%>\
                % endfor
)
        % endfor
;
% endfor

    % for type_name, constructors in types.get_types().items():
        % if type_name in RascalContext.types_extended_from_m3:
data ${type_name} =\
        % else:
data ${type_name}(loc src=|unknown:///|) =\
        % endif
<%
         vertical_bar = " "
%>\
        % for constructor in constructors:
${vertical_bar}${constructor.get_name()} (\
<%
             vertical_bar = "| "
             comma = ""
%>\
            % for field_name, field_type in constructor.get_fields().items():
${comma}${field_type} ${field_name}\
<%
                  comma = ", "
%>\
             % endfor
)
          % endfor
;
     % endfor
