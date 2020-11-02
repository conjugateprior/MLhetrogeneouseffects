/* This .ado file creates betas that can be directly used in tables
and takes global (numeric) as inputs */

# delimit;


capture program drop betaformat;
program define betaformat, sortpreserve;
    version 9.2;
    syntax namelist;
    
    capture drop helppp*;

    local zpz=1;
    foreach el of local namelist {;

        cap drop helppp`zpz'
        tempvar helppp`zpz';
        qui gen helppp`zpz'=.;

        qui replace helppp`zpz'=$`el' in 1;

        if $`el'>-1 & $`el'<1 {;
            qui format helppp`zpz' %9.3f;
            if $`el'>-0.001 & $`el'<0.001 qui format helppp`zpz' %9.4f;
            if $`el'>-0.0001 & $`el'<0.0001 qui format helppp`zpz' %9.5f;
            if $`el'>=0.999 & $`el'<1 qui replace helppp`zpz'=0.999;
            if $`el'<=-0.999 & $`el'>-1 qui replace helppp`zpz'=-0.999;
            local status "zero";
            local sign "";
            if $`el'<0 local sign "minus";
        };
        if ($`el'>-10 & $`el'<=-1) | ($`el'<10  & $`el'>=1) qui format helppp`zpz' %9.2f;
        if ($`el'>-100 & $`el'<=-10) | ($`el'<100 & $`el'>=10) qui format helppp`zpz' %9.1f;
        if $`el'<=-100 | $`el'>=100 qui format helppp`zpz' %9.0f;
        if ($`el'>-1000000 & $`el'<=-1000) | ($`el'<1000000 & $`el'>=1000) local status "thousand";
        if ($`el'>-1000000000 & $`el'<=-1000000) | ($`el'<1000000000 & $`el'>=1000000) local status "million";

        qui tostring helppp`zpz', replace force usedis;
        global `el'=helppp`zpz'[1];
        global `el'="$`el'";

        if "`status'"=="zero" {;
            if "`sign'"=="" global `el'=substr("$`el'",2,.);
            if "`sign'"=="minus" {;
                global `el'=substr("$`el'",3,.);
                global `el'="-$`el'";
                local sign "";
            };
            local status "";
        };
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
