$SqlBits16 = Invoke-WebRequest "http://www.sqlbits.com/information/PublicSessions"

$Items = $SqlBits16.AllElements | `
Where-Object {($_.Class -like "col-sm-*") -or ($_.Class -eq "panel-body")  } | `
% { '"' + $_.innerText.replace([Environment]::NewLine, ' ').Replace('"','').Trim() + '"'}

0..($Items.Length/4-1) | `
% { -join ($Items[($_*4)], ',', $Items[($_*4+1)], ',', $Items[($_*4+2)].Replace(' ', '","'), ',', $Items[($_*4+3)]) } | `
Out-File 'D:\GitHub\SqlBits\Data\SqlBits16.csv' -Force -Encoding 'utf8'