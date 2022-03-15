/**
* CDO utility scripts
*
*/
var cdo={
	//methods adEvent
	 addEvent: function(obj,evt,fn){
			if(obj.addEventListener){
				obj.addEventListener(evt, fn, false);
			}
			else if(obj.attachEvent){
				obj.attachEvent('on' + evt,fn);
			}
		},
	hasClass: function(ele,cls) {
    	return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
    },
    addClass: function(ele,cls) {
	if (!cdo.hasClass(ele,cls)) ele.className += " "+cls;
    },
    removeClass: function(ele,cls) {
		if (cdo.hasClass(ele,cls)) {
			var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
			ele.className=ele.className.replace(reg,' ');
		}
    },
   show: function (id){
        for(var i=0; i< arguments.length; i++){
            var el = arguments[i];
            if(document.getElementById(el)){
                document.getElementById(el).style.display = 'block';
            }
        }
    },
    hide: function (id){
       for(var i=0; i< arguments.length; i++){
            var el = arguments[i];
            if(document.getElementById(el)){
                document.getElementById(el).style.display = 'none';
            }
        }
    },
    toggle: function(id){
		if(!document.getElementById(id)){ return;}
        document.getElementById(id).style.display == "none"? document.getElementById(id).style.display = "block":document.getElementById(id).style.display = "none";
    },
   //trim leading, trailing and multiple white spaces
   trim: function(str){
      str = str.replace(/(^\s*)|(\s*$)/gi,"");
      str = str.replace(/[ ]{2,}/gi," ");
      return str;
   },
   pad: function(val, len) {
        val = String(val);
        len = len || 2;
        while (val.length < len) {val = "0" + val;}
        return val;
    },

    loading:  {
        //methods: pageLoad, slowLoad, timeoutId
		//config:{
			//content and insertId for dynamic screen
			content: "<p>Please wait</p>", //message or additional html content
			insertId: 'content-block' || 'content', //relative postioned parent 
			
			wait: 4000,
			loadingScreenId: 'loading', //better as 'loadingScreen
		//},
		    pageLoaded:'waiting',
		
		//after page loaded
		pageLoad: function(){
			cdo.loading.pageLoaded='loaded';
			cdo.hide(cdo.loading.loadingScreenId);
		},
		
		slowLoad: function(){
			//if page isn't loaded make/show a loadScreen
			if(cdo.loading.pageLoaded !=='loaded'){
				cdo.loading.loadscreen();
			}
			else {
				clearTimeout(cdo.loading.timeoutId);
				cdo.hide(cdo.loading.loadingScreenId);
			}
		},  
	    //change time to a variable (4 secs for mid size, 8 secs for significant applications/interfaces)
		timeoutId:  function(){setTimeout(cdo.loading.slowLoad, 4000);},
		
		//check if loadingScreen exists or make, then show
		loadscreen: function(){		
				cdo.show(cdo.loading.loadingScreenId);
	    } 	
	}		
}
cdo.loading.timeoutId();
window.onLoad = cdo.addEvent( window, 'load', cdo.loading.pageLoad);