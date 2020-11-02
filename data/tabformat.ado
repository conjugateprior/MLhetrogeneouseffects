/* Transfers numerical variables into a format suitable for tables */

# delimit;

capture program drop tabformat;
program define tabformat, sortpreserve;
    version 9.2;
    syntax namelist;
    
    qui count;
    local xmaxx=r(N);
    foreach el of local namelist {;
        local nxxn=1;
        qui gen `el'_new="";
        while `nxxn'<=`xmaxx' {;
            global cnum=`el'[`nxxn'];
            if "$cnum"=="." qui replace `el'_new="" in `nxxn';
            if "$cnum"!="." {;
                betaformat cnum;
                qui replace `el'_new="$cnum" in `nxxn';
            };
            local nxxn=`nxxn'+1;
        };
    };
    local drop nxxn xmaxx;
    global drop cnum;
    
end;
