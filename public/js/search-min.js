function getFile(a,d){var c=a;var b;if(window.XMLHttpRequest){b=new XMLHttpRequest()}else{b=new ActiveXObject("Microsoft.XMLHTTP")}b.onreadystatechange=function(){if(b.readyState==1){if(b.overrideMimeType){b.overrideMimeType("application/json");b.setRequestHeader("Accept","application/vnd.github.v3.text-match+json")}b.send()}if(b.readyState==4){if(b.status===200||b.status===0){d(b.responseText)}}};b.open("GET",c,true)}function mapURL(a){var b="http://sysresearcher.github.io/";var j="_posts/";if(a.lastIndexOf(j,0)===0){var i=a.split("-");i.splice(0,3);return b+i.join("-").replace(".md","")}var f="/about";var h="/contact";if(a.lastIndexOf(f,0)===0||a.lastIndexOf(h,0)===0){return b+a.replace(".md","")}var e="strategy/";var c="/strategy";var g="tools/";var d="/tools";if(a.lastIndexOf(e,0)===0||a.lastIndexOf(g,0)===0||a.lastIndexOf(c,0)===0||a.lastIndexOf(d,0)===0){return b+a.replace(".md",".html")}return b+a}function doSearch(){var b=document.getElementById("search").value;var a="https://api.github.com/search/code?q='"+b+"' in:file language:markdown repo:sysresearcher/sysresearcher.github.io&sort=indexed&order=desc";getFile(a,function(d){var i=JSON.parse(d);var g=["<br><div>Found "+i.total_count.toString()+" match(es).</div>"];if(i.items&&i.items.length>0){g.push("<h2>Results:</h2>");var f,c;for(f=0;f<i.items.length;++f){var h=i.items[f];g.push("<div><h3><a href='"+mapURL(h.path)+"'></h3>"+h.name.replace(".md","")+"</a>");if(h.text_matches&&h.text_matches.length>0){g.push("<ul>");for(c=0;c<h.text_matches.length;++c){var e=h.text_matches[c].fragment;e=e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;");g.push("<li>"+e.replace(new RegExp(b,"gi"),"<b>"+b+"</b>")+"</li>")}g.push("</ul>")}g.push("</div>")}}document.getElementById("output").innerHTML=g.join("")});return};