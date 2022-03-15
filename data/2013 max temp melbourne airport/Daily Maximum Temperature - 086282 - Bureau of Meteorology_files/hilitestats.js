// For CDO climate data tabular display - solar revision
// Last modified 27 April 2011
//reconciles hilitedaily.js hilitestats.js hilitestats-solar.js + extras

function clearOptionList(list){
      list.selectedIndex = -1;
      list.options.length = 0;
}

function comparator (isvisible) { 
    if (isvisible === "show") {
        document.getElementById("table-options").style.display ="block";
        document.getElementById("showComparator").style.display ="none";
        document.getElementById("hideComparator").style.display ="block";
    }
    else{
        document.getElementById("table-options").style.display ="none";
        document.getElementById("hideComparator").style.display ="none";
        document.getElementById("showComparator").style.display ="block";
    }
}

//from daily file
function highlightDaily() {
	//1 threshold >=, -1 threshold <=
	var threshold;
	var dataTable = document.getElementById('dataTable');
	var value;
	var period = "notSelected";
	//var dataCode = document.getElementById('p_nccObsCode').value + ''|| ''; //?
	var dataCode = document.getElementById('p_nccObsCode').value;
	
	var displayType = document.getElementById('p_display_type').value;
	var selectedIndex=document.getElementById("statType").selectedIndex;
	var selectedText=document.getElementById("statType").options[selectedIndex].text;
	threshold = parseFloat(selectedText.replace(/[^-^0-9]/g, ''));
	function hasClass(ele,cls) {
	return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
	}
	function addClass(ele,cls) {
		if (!hasClass(ele,cls)) ele.className += " "+cls;
	}
	function removeClass(ele,cls) {
		if (hasClass(ele,cls)) {
			var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
			ele.className=ele.className.replace(reg,' ');
		}
	}

	try{
		if(document.getElementById){  
			for(var i = 2; i<33; i++){
				for (var j=1; j<13; j++){
					if (dataTable.rows[i].cells[j].firstChild != null){
						value = parseFloat(dataTable.rows[i].cells[j].firstChild.nodeValue);
						if(selectedIndex === 0 || selectedText ==''){
							removeClass(dataTable.rows[i].cells[j],"cell_shade");
						}
						else if(dataCode !== '123' && value >= threshold){
								addClass(dataTable.rows[i].cells[j],"cell_shade");
						}
						else if(dataCode === '123' && value <= threshold){
							addClass(dataTable.rows[i].cells[j],"cell_shade");
						}
						else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
					}
				}
			}
		}
	}
	catch(e){
		if(e instanceof TypeError){ var message='Variable type is not correct!';}
		else if(e instanceof ReferenceError){ var message='Incorrect reference!';}
		else if(e instanceof RangeError){ var message='Value is out of range!';}
		else{ var message='Unknown error!';}
		var p=document.createElement('p');
		p.appendChild(document.createTextNode('The following exception was thrown by the script :'+message+' Error name :'+e.name+' Error message :'+e.message));
		//document.body.appendChild(p);
	}
}
//end daily

function highlight() {
    var dataTable = document.getElementById('dataTable');
    if(document.getElementById('statsTable')){
        var statTable = document.getElementById('statsTable');
    }
    var value;
    var period = "all";//"notSelected";
	//var _p=''; //period suffix for normals
    var cellText;
    var nodeValue;
    var colNum = dataTable.rows[0].cells.length;
    var textPattern;

    var selectedIndex=document.getElementById("statType").selectedIndex;
    var selectedText=document.getElementById("statType").options[selectedIndex].text;
    var cellattr, rowattr, idattr;
    
    if(document.selection.refPeriod){
       document.selection.refPeriod[1].checked === true ? period = "normals" : period = "all";
	   period ==="normals" ? _p='_30' : _p='';
    }
    
    var oRows = document.getElementById('dataTable').getElementsByTagName('tr');
    var iRowCount = oRows.length;
    //reduce length for dailytable to avoid daily summary
    if(colNum ===13){iRowCount = iRowCount-3;}
    
    function hasClass(ele,cls) {
        return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
    }
    function addClass(ele,cls) {
        if (!hasClass(ele,cls)) {ele.className += " "+cls;}
    }
    function removeClass(ele,cls) {
        if (hasClass(ele,cls)) {
            var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
            ele.className=ele.className.replace(reg,' ');
        }
    }
    
    try{
            for(var i = 2; i<iRowCount; i++){
                 cellattr= dataTable.rows[i].cells[0].getAttribute('colspan');
                 rowattr =  dataTable.rows[i].getAttribute('class');
				 
				 //monthly
                 if((cellattr != '14') && (rowattr != 'year_row')){
                    for (var j=1; j< colNum; j++){
                        if (dataTable.rows[i].cells[j].firstChild !== null){
                            value = parseFloat(dataTable.rows[i].cells[j].firstChild.nodeValue);
                            if(selectedIndex === 0 || selectedText ===''){
                                removeClass(dataTable.rows[i].cells[j],"cell_shade"); 
                            }
							
                            if(period == "all"){ 
                                if (selectedText == "Lowest"){
                                    if (lowest[j] != '' && value == lowest[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "5th percentile or less"){
                                    if (value<=percentile5[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "10th percentile or less"){
                                    if (value<=percentile10[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "Median or less"){
                                    if (value<=median[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText.search(/Mean or less/i) !== -1){
                                    if (mean[j] !== '' && value<=mean[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText.search(/Mean or more/i) !== -1){
                                    if (mean[j] !== '' && value >=mean[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "Median or more"){
                                    if (value>=median[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "90th percentile or more"){
                                    if (value>=percentile90[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "95th percentile or more"){
                                    if (value>=percentile95[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "Highest"){
                                    if (highest[j] !== '' && value==highest[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                            }
                            
							else if(period =="normals"){
                               if (selectedText == "Lowest"){								
                                    if (value==lowest_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "5th percentile or less"){
                                    if (value<=percentile5_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "10th percentile or less"){
                                    if (value<=percentile10_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "Median or less"){
                                    if (value<=median_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText.search(/Mean or less/i) !== -1){
                                    if (value<=mean_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText.search(/Mean or more/i) !== -1){
                                    if (value>=mean_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "Median or more"){
                                    if (value>=median_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "90th percentile or more"){
                                    if (value>=percentile90_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "95th percentile or more"){
                                    if (value>=percentile95_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                }
                                else if (selectedText == "Highest"){
                                    if (value==highest_30[j]){addClass(dataTable.rows[i].cells[j],"cell_shade");}
                                    else{removeClass(dataTable.rows[i].cells[j],"cell_shade");}
                                } 
                           }
                        }
                    }
                }
            }
    }
    catch(e){
        var message;
        if(e instanceof TypeError){ message='Variable type is not correct!';}
        else if(e instanceof ReferenceError){ message='Incorrect reference!';}
        else if(e instanceof RangeError){ message='Value is out of range!';}
        else{ message='Unknown error!';}
        var p=document.createElement('p');
        p.appendChild(document.createTextNode('The following exception was thrown by the script :'+message+' Error name :'+e.name+' Error message :'+e.message));
        //document.body.appendChild(p);
    }
}

//called from period select buttons
function updateStats(){
    if(!document.getElementById('statsTable')){return;}
    //remove to constants
    var statTable = document.getElementById("statsTable");
    var statList = document.getElementById("statType");
    var selectedIndex = statList.selectedIndex;
	
    if(document.selection.refPeriod[0].checked){
        document.getElementById('p1961-1990').style.display="none";
        document.getElementById('all_years').style.display="block";
        
        for (var j=0; j<14; j++){
            statTable.rows[1].cells[j].firstChild.nodeValue=mean[j];
            statTable.rows[2].cells[j].firstChild.nodeValue=lowest[j];
            statTable.rows[3].cells[j].firstChild.nodeValue=percentile5[j];
            statTable.rows[4].cells[j].firstChild.nodeValue=percentile10[j];
            statTable.rows[5].cells[j].firstChild.nodeValue=median[j];
            statTable.rows[6].cells[j].firstChild.nodeValue=percentile90[j];
            statTable.rows[7].cells[j].firstChild.nodeValue=percentile95[j];
            statTable.rows[8].cells[j].firstChild.nodeValue=highest[j];
        }
        
        //find error for normals
        clearOptionList(document.getElementById("statType"));
        statList.options[0]=new Option("No highlight", "No highlight");
        statList.options[1]=new Option("Lowest", "Lowest");
        statList.options[2]=new Option("5th percentile or less", "5th percentile or less");
        statList.options[3]=new Option("10th percentile or less", "10th percentile or less");
        statList.options[4]=new Option("Median or less", "Median or less");
        statList.options[5]=new Option("Mean or less", "Mean or less");
        statList.options[6]=new Option("Mean or more", "Mean or more");
        statList.options[7]=new Option("Median or more", "Median or more");
        statList.options[8]=new Option("90th percentile or more", "90th percentile or more");
        statList.options[9]=new Option("95th percentile or more", "90th percentile or more");
        statList.options[10]=new Option("Highest", "Highest");
        if(selectedIndex > 0){
            statList.selectedIndex = selectedIndex+1;
        }
    }
     
    else {
        document.getElementById('p1961-1990').style.display="block";
        document.getElementById('all_years').style.display="none";
        for (var j=0; j<14; j++){
            statTable.rows[1].cells[j].firstChild.nodeValue=mean_30[j];
            statTable.rows[2].cells[j].firstChild.nodeValue=lowest_30[j];
            statTable.rows[3].cells[j].firstChild.nodeValue=percentile5_30[j];
            statTable.rows[4].cells[j].firstChild.nodeValue=percentile10_30[j];
            statTable.rows[5].cells[j].firstChild.nodeValue=median_30[j];
            statTable.rows[6].cells[j].firstChild.nodeValue=percentile90_30[j];
            statTable.rows[7].cells[j].firstChild.nodeValue=percentile95_30[j];
            statTable.rows[8].cells[j].firstChild.nodeValue=highest_30[j];
        }       
       //statList.options.length=0; //repopulate this list without lowest and highest elements
        clearOptionList(document.getElementById("statType"));
        statList.options[0]=new Option("No highlight", "No highlight");
        statList.options[1]=new Option("5th percentile or less", "5th percentile or less");
        statList.options[2]=new Option("10th percentile or less", "10th percentile or less");
        statList.options[3]=new Option("Median or less", "Median or less");
        statList.options[4]=new Option("Mean or less", "Mean or less");
        statList.options[5]=new Option("Mean or more", "Mean or more");
        statList.options[6]=new Option("Median or more", "Median or more");
        statList.options[7]=new Option("90th percentile or more", "90th percentile or more");
        statList.options[8]=new Option("95th percentile or more", "90th percentile or more");
   
        if ((selectedIndex===1) || (selectedIndex===10)){ //lowest or highest then set to no hilite
            statList.selectedIndex = 0;
        }
        else{
            if(selectedIndex > 0) {
                statList.selectedIndex = selectedIndex-1;
            }
        }
    }
    highlight();
}

//call for relevant ids - ie daily code '136'
function doHighlightArray(){
    //call function to make the highlight value array
    lowest =[];
    mean=[];
    highest=[];
    var t;
    if(!document.getElementById('statsTable')){return;}
    t = document.getElementById('statsTable');
   
    var m,h,l, rowlen;
    rowlen = t.rows.length;
    for (var i=0;i<13 ;i++){
        if(rowlen===4){
            if(t.rows[1].cells[i].firstChild){m = t.rows[1].cells[i].firstChild.nodeValue;}
            else {m='';}
            mean.push(m);
        }
        if(t.rows[rowlen-2].cells[i].firstChild){h = t.rows[rowlen-2].cells[i].firstChild.nodeValue;}
        else {h='';}
        highest.push(h);
        if(t.rows[rowlen-1].cells[i].firstChild){l = t.rows[rowlen-1].cells[i].firstChild.nodeValue;}
        else {l='';}
        lowest.push(l);
    }
}

function initHighlightStats(){ 
    var displayType = document.getElementById('p_display_type').value;
	var dataCode = document.getElementById('p_nccObsCode').value;
    if(document.getElementById("p_startYear")){
        document.getElementById("p_startYear").onchange = doForm;
    }
    if(document.getElementById('dr_year')){
		var button = document.getElementById('viewDailyYear');
        button.onclick= doYearFromMonthlyForm; 
    }
    if(dataCode === '193'){    
            doHighlightArray();
    }
    if(document.getElementById("statType")){
        document.getElementById("statType").selectedIndex = 0;
        document.getElementById("statType").onchange = function(){ 
			//daily
			if(displayType === 'dailyDataFile' && dataCode !== '193'){
				 highlightDaily(); 
				}
			//monthly
			else{
				highlight();
			}
        };
    } 
    //stats period
   if(typeof(document.forms['selection'].elements['refPeriod'])!== "undefined"){
        var refPeriod = document.forms['selection'].elements['refPeriod'][0];
        document.forms['selection'].elements['refPeriod'][0].onclick=function(){
            if(refPeriod === this.value){return;}
            refPeriod= this.value;
            updateStats();
        }
        
		document.forms['selection'].elements['refPeriod'][1].onclick=function(){
		   if(refPeriod === this.value){return;}
		   refPeriod= this.value;
		   updateStats();
        }
    }
}

function addEvent(obj,evt,fn){
    if(obj.addEventListener){
        obj.addEventListener(evt, fn, false);
    }
    else if(obj.attachEvent){
        obj.attachEvent('on' + evt,fn);
    }
}

window.onLoad = addEvent(window, 'load', initHighlightStats );
//daily change year
function doForm(){ 
    if(document.getElementById('selection')){
        var f, year, p_s;
        f    = document.getElementById('selection');
        year = f.p_startYear.value.substring(0,4);
        p_c = f.p_startYear.value.substring(5);
        path='/jsp/ncc/cdio/weatherData/av?p_nccObsCode='+ f.p_nccObsCode.value +'&p_display_type='+ f.p_display_type.value +'&p_startYear='+ year +'&p_c='+ p_c +'&p_stn_num='+ f.p_stn_num.value;
        window.location = path;
        //document.selection.submit(); 
        return false;
    }
}

//from monthly form go to daily. Two forms in monthly page.
function doYearFromMonthlyForm(){ 
   if(document.getElementById('selection') && document.getElementById('monthly_yr')){
        var f, year, p_s, monthlyCode, dailyCode;
        f    = document.getElementById('selection');
        f2    = document.getElementById('monthly_yr');
        year = f2.dr_year.value.substring(0,4);
        p_c =  f2.dr_year.value.substring(5);
        monthlyCode = f.p_nccObsCode.value;
        switch(monthlyCode){
            case '139' :
                dailyCode = 136;//rain
                break;
            case '36':
                dailyCode = 122; //max
                break;
            case '40':
                 dailyCode = 122;
                 break;
            case '41':
                 dailyCode = 122;
                 break;
                 
            case '38':
                 dailyCode = 123;//min
                 break;
            case '43':
                dailyCode = 123;
                break;
            case '42':
                dailyCode = 123;
                break;
            case '203': 
                dailyCode = 193;//solar
                break;
            default: return; //Error handler...
        }
        path='/jsp/ncc/cdio/weatherData/av?p_nccObsCode='+ dailyCode +'&p_display_type=dailyDataFile&p_startYear='+ year +'&p_c='+ p_c +'&p_stn_num='+ f.p_stn_num.value;
        window.location = path;
        
        //document.selection.submit(); 
        return false;
    }
}
