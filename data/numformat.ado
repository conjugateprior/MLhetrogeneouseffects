/* This .ado file creates numbers/amounts that can be directly used in tables
and takes global (numeric) as inputs */

# delimit;


capture program drop numformat;
program define numformat, sortpreserve;
    version 9.2;
    syntax namelist;
    
    capture drop helppp*;

    local zpz=1;
    foreach el of local namelist {;

        cap drop helppp`zpz'
        tempvar helppp`zpz';
        qui gen helppp`zpz'=.;

        qui replace helppp`zpz'=$`el' in 1;

        if $`el'>-1000 & $`el'<1000 {;
            qui format helppp`zpz' %9.0f;
            local status "";
        };
        if ($`el'>-1000000 & $`el'<=-1000) | ($`el'<1000000 & $`el'>=1000) local status "thousand";
        if ($`el'>-1000000000 & $`el'<=-1000000) | ($`el'<1000000000 & $`el'>=1000000) local status "million";

        qui tostring helppp`zpz', replace force usedis;
        global `el'=helppp`zpz'[1];
        global `el'="$`el'";

        if "`status'"=="thousand" {;
            local lppp=length("$`el'")-3;
            local p1pp=substr("$`el'",1,`lppp');
            local p2pp=substr("$`el'",`lppp'+1,.);
            global `el'="`p1pp',`p2pp'";
            local status "";
        };
        if "`status'"=="million" {;
            local lppp=length("$`el'")-6;
            local p1pp=substr("$`el'",1,`lppp');
            local p2pp=substr("$`el'",-6,3);
            local p3pp=substr("$`el'",-3,.);
            global `el'="`p1pp',`p2pp',`p3pp'";
            local status "";
        };
        drop helppp`zpz';
        local zpz=`zpz'+1;
        
    };
local drop zpz p1pp p2pp p3pp;
end;
