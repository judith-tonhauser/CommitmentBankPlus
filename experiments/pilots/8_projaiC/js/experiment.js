function make_slides(f) {
  var   slides = {};

  slides.botcaptcha = slide({
      name : "botcaptcha",
      start: function() {

      // define possible speaker and listener names
      // fun fact: 10 most popular names for boys and girls
      var speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      var listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];

      var story = speaker + ' says to ' + listener + ': "It\'s a beautiful day, isn\'t it?"' + '<br><br><br><br> Who is ' + speaker + ' talking to? Write the name into the box.';

      $("#story").html(story);

      // don't allow enter press in text field
      $('#listener-response').keypress(function(event) {
          if (event.keyCode == 13) {
              event.preventDefault();
          }
      });

      // don't show any error message
      $("#error").hide();
      $("#error_incorrect").hide();
      $("#error_2more").hide();
      $("#error_1more").hide();

      // amount of trials to enter correct response
      var trial = 0;

      // when button is pressed
      $("#next").on("click", function() {

        // get rid of spaces in response
        response = $("#listener-response").val().replace(" ","");

        // response correct
        if (listener.toLowerCase() == response.toLowerCase()) {
            // I always save their response globally in the data, but I don't know
            // whether you want that
            exp.go();

        // response false
        } else {
            trial = trial + 1;
            $("#error_incorrect").show();
            if (trial == 1) {
                $("#error_2more").show();
            } else if (trial == 2) {
                $("#error_2more").hide();
                $("#error_1more").show();
            } else {
                // incorrect response on third try
                $("#error_incorrect").hide();
                $("#error_1more").hide();
                // remove button, so that the participant can't advance
                $("#next").hide();
                // deactivate text field
                $('#listener-response').css("opacity", "0.2");
                $('#listener-response').prop("disabled", true);
                $("#error").show();
            };
        };
            
        });

    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });
  
  slides.instructions1 = slide({
    name : "instructions1",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    	var inst1 = "";
//    	console.log(block_order);
    	if (exp.stims_block1[0].block == "ai") {
    		inst1 = inst1 + "First you'll answer questions about whether one person's response to another person sounds good."
    	} else {
    		inst1 = inst1 + "First you'll answer questions about what the people at the party are certain about."    		
    		}
    	$("#inst1").html(inst1);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     

  slides.block1 = slide({
    name : "block1",
    present : exp.stims_block1,
    start : function() {
      $(".err").hide();
    },
    present_handle : function(stim) {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	 
      console.log(this.stim); 
//      var youSure = this.stim.name2 + ": Are you sure?<br><br>";
//	  var answer = this.stim.name + ": Yes, I am sure that "+this.stim.question+".<br><br>";
//	  var utterance = "<table><tr><td><strong>"+this.stim.name + ":</strong> \"<i>"+this.stim.utterance+"</i>\"</td></tr><tr><td><strong>"+this.stim.name2 + ":</strong> \"<i>Are you sure?</i>\""+"</td></tr><tr><td><strong>"+this.stim.name + ": </strong> \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""+"</td></tr></table>"
      var utterance = "";
      if (this.stim.block == "ai") {
	  		utterance = "<table><tr><td><strong>"+this.stim.name + ":</strong> \"<i>" +this.stim.utterance+"</i>\"</td></tr><tr><td><strong>"+this.stim.name2 + ":</strong> \"<i>Yes, that's true, " + this.stim.question +".</i>\"";
        // controls: 
        // Name: These muffins have blueberries in them.  
        // Name2: Are you sure?
        // Name: Yes, I am sure that these muffins have blueberries in them.


	  } else {
	  		utterance = "<strong>"+this.stim.name+": </strong>\"<i>"+this.stim.utterance+"</i>\"";	  	
	  	}
      //if (this.stim.block == "ai") {
	  //		utterance = this.stim.name + " says to " + this.stim.name2 + ": \"<strong><i>"+this.stim.utterance+"</i></strong>\"<br><br>" + youSure + answer;
	  //} else {
	  //		utterance = this.stim.name + " says to " + this.stim.name2 + ": \"<strong><i>"+this.stim.utterance+"</i></strong>\"";	  	
	  	//}
	  $(".sentence").html(utterance);
	  var question = "";
	  console.log(this.stim.block);
	  if (this.stim.block == "ai") {
	  		question = "Does "+this.stim.name2+"'s response to "+this.stim.name+" sound good?";
	  } else {
	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	 
        //controls:
        //Is NAME certain that these muffins have blueberries in them?

	  	}
	  $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block1",
      "question_type" : this.stim.block,      
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  
  slides.instructions2 = slide({
    name : "instructions2",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
    	var inst2 = "That was the first half! ";
    	if (exp.stims_block2[0].block == "ai") {
    		inst2 = inst2 + "Now you'll answer questions about whether one person's response to another person sounds good."
    	} else {
    		inst2 = inst2 + "Now you'll answer questions about what the people at the party are certain about."    		
    		}
    	$("#inst2").html(inst2);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });   
  
  slides.block2 = slide({
    name : "block2",
    present : exp.stims_block2,
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
      $(".err").hide();
    },
    present_handle : function(stim) {
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	
      var utterance = "";
      if (this.stim.block == "ai") {
	  		utterance = "<table><tr><td><strong>"+this.stim.name + ":</strong> \"<i>" +this.stim.utterance+"</i>\"</td></tr><tr><td><strong>"+this.stim.name2 + ":</strong> \"<i>Yes, that's true, " + this.stim.question +".</i>\"";
	  } else {
	  		utterance = "<strong>"+this.stim.name+": </strong>\"<i>"+this.stim.utterance+"</i>\"";	  	
	  	}      
	  $(".sentence").html(utterance);
	  var question = "";
	  console.log(this.stim.block);	  
	  if (this.stim.block == "ai") {
	  		question = "Does "+this.stim.name2+"'s response to "+this.stim.name+" sound good?";
	  } else {
	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	  	
	  	}
	  $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider2", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block2",
      "question_type" : this.stim.block,     
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,   	  
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  });        
 

  slides.questionaire =  slide({
    name : "questionaire",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
//        enjoyment : $("#enjoyment").val(),
//        asses : $('input[name="assess"]:checked').val(),
        american : $('input[name="ame"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
//        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.finished = slide({
    name : "finished",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      //setTimeout(function() {turk.submit(exp.data);}, 1000);
      proliferate.submit(exp.data);
    }
  });

  return slides;
}

/// init ///
function init() {

  var names = _.shuffle([
    {
      "name":"James",
      "gender":"M"
    },
//    {
//      "name":"John",
//      "gender":"M"
//    },
    {
      "name":"Robert",
      "gender":"M"
    },
//     {
//       "name":"Michael",
//       "gender":"M"
//     },
    {
      "name":"William",
      "gender":"M"
    },
    {
      "name":"David",
      "gender":"M"
    },
//    {
//      "name":"Richard",
//      "gender":"M"
//    },
    {
      "name":"Joseph",
      "gender":"M"
    },
    {
      "name":"Charles",
      "gender":"M"
    },
    {
      "name":"Thomas",
      "gender":"M"
    },
    {
      "name":"Christopher",
      "gender":"M"
    },
    {
      "name":"Daniel",
      "gender":"M"
    },
    {
      "name":"Matthew",
      "gender":"M"
    },
//    {
//      "name":"Donald",
//      "gender":"M"
//    },
    {
      "name":"Anthony",
      "gender":"M"
    },
    {
      "name":"Paul",
      "gender":"M"
    },
//    {
//      "name":"Mark",
//      "gender":"M"
//    },
    {
      "name":"George",
      "gender":"M"
    },
    {
      "name":"Steven",
      "gender":"M"
    },
    {
      "name":"Kenneth",
      "gender":"M"
    },
//    {
//      "name":"Andrew",
//      "gender":"M"
//    },
    {
      "name":"Edward",
      "gender":"M"
    },
//     {
//       "name":"Joshua",
//       "gender":"M"
//     },
    {
      "name":"Brian",
      "gender":"M"
    },
    {
      "name":"Kevin",
      "gender":"M"
    },
    {
      "name":"Ronald",
      "gender":"M"
    },
    {
      "name":"Timothy",
      "gender":"M"
    },
    {
      "name":"Jason",
      "gender":"M"
    },
    {
      "name":"Jeffrey",
      "gender":"M"
    },
    {
      "name":"Gary",
      "gender":"M"
    },
    {
      "name":"Ryan",
      "gender":"M"
    },
    {
      "name":"Nicholas",
      "gender":"M"
    },
    {
      "name":"Eric",
      "gender":"M"
    },
    {
      "name":"Jacob",
      "gender":"M"
    },
    {
      "name":"Jonathan",
      "gender":"M"
    },
    {
      "name":"Larry",
      "gender":"M"
    },
//    {
//      "name":"Frank",
//      "gender":"M"
//    },
    {
      "name":"Scott",
      "gender":"M"
    },
    {
      "name":"Justin",
      "gender":"M"
    },
    {
      "name":"Brandon",
      "gender":"M"
    },
    {
      "name":"Raymond",
      "gender":"M"
    },
    {
      "name":"Gregory",
      "gender":"M"
    },
    {
      "name":"Samuel",
      "gender":"M"
    },
    {
      "name":"Benjamin",
      "gender":"M"
    },
    {
      "name":"Patrick",
      "gender":"M"
    },
//    {
//      "name":"Jack",
//      "gender":"M"
//    },
    {
      "name":"Dennis",
      "gender":"M"
    },
    {
      "name":"Jerry",
      "gender":"M"
    },
    {
      "name":"Alexander",
      "gender":"M"
    },
    {
      "name":"Tyler",
      "gender":"M"
    },
//    {
//      "name":"Mary",
//      "gender":"F"
//    },
    {
      "name":"Jennifer",
      "gender":"F"
    },
    {
      "name":"Elizabeth",
      "gender":"F"
    },
    {
      "name":"Linda",
      "gender":"F"
    },
    //{
    //  "name":"Emily",
    //  "gender":"F"
    //},
    {
      "name":"Erin",
      "gender":"F"
    },
//    {
//      "name":"Susan",
//      "gender":"F"
//    },
    {
      "name":"Margaret",
      "gender":"F"
    },
    {
      "name":"Jessica",
      "gender":"F"
    },
    {
      "name":"Dorothy",
      "gender":"F"
    },
//     {
//       "name":"Sarah",
//       "gender":"F"
//     },
    {
      "name":"Karen",
      "gender":"F"
    },
    {
      "name":"Nancy",
      "gender":"F"
    },
//     {
//       "name":"Betty",
//       "gender":"F"
//     },
    {
      "name":"Lisa",
      "gender":"F"
    },
    {
      "name":"Sandra",
      "gender":"F"
    },
//     {
//       "name":"Helen",
//       "gender":"F"
//     },
    {
      "name":"Ashley",
      "gender":"F"
    },
    {
      "name":"Donna",
      "gender":"F"
    },
    {
      "name":"Kimberly",
      "gender":"F"
    },
    {
      "name":"Carol",
      "gender":"F"
    },
    {
      "name":"Michelle",
      "gender":"F"
    },
        //{
    //  "name":"Emily",
    //  "gender":"F"
    //},
    {
      "name":"Fran",
      "gender":"F"
    },
//     {
//       "name":"Amanda",
//       "gender":"F"
//     },
    {
      "name":"Melissa",
      "gender":"F"
    },
    {
      "name":"Deborah",
      "gender":"F"
    },
    {
      "name":"Laura",
      "gender":"F"
    },
    {
      "name":"Stephanie",
      "gender":"F"
    },
    {
      "name":"Rebecca",
      "gender":"F"
    },
    {
      "name":"Sharon",
      "gender":"F"
    },
    {
      "name":"Cynthia",
      "gender":"F"
    },
    {
      "name":"Kathleen",
      "gender":"F"
    },
    {
      "name":"Ruth",
      "gender":"F"
    },
//    {
//      "name":"Anna",
//      "gender":"F"
//    },
    {
      "name":"Shirley",
      "gender":"F"
    },
    {
      "name":"Amy",
      "gender":"F"
    },
    {
      "name":"Angela",
      "gender":"F"
    },
    {
      "name":"Virginia",
      "gender":"F"
    },
    {
      "name":"Brenda",
      "gender":"F"
    },
 //    {
//       "name":"Catherine",
//       "gender":"F"
//     },
    {
      "name":"Nicole",
      "gender":"F"
    },
    {
      "name":"Christina",
      "gender":"F"
    },
//     {
//       "name":"Janet",
//       "gender":"F"
//     },
//     {
//       "name":"Samantha",
//       "gender":"F"
//     },
    {
      "name":"Carolyn",
      "gender":"F"
    },
    {
      "name":"Rachel",
      "gender":"F"
    },
    {
      "name":"Heather",
      "gender":"F"
    },
    {
      "name":"Diane",
      "gender":"F"
    },
//     {
//       "name":"Joyce",
//       "gender":"F"
//     },
    {
      "name":"Julie",
      "gender":"F"
//     },
//     {
//       "name":"Emma",
//       "gender":"F"
    }
  ]);
  
//12 names for the 6 main clause contents
  
var mcnames = _.shuffle([
    {
      "name":"Finn",
      "gender":"M"
    },
    {
      "name":"Marlo",
      "gender":"M"
    },
    {
      "name":"Pax",
      "gender":"M"
    },
    {
      "name":"Ike",
      "gender":"M"
    },
    {
      "name":"Dalton",
      "gender":"M"
    },
    {
      "name":"Arlen",
      "gender":"M"
    },
    {
      "name":"Leia",
      "gender":"F"
    },
    {
      "name":"Cici",
      "gender":"F"
    },
    {
      "name":"Saskia",
      "gender":"F"
    },
    {
      "name":"Autumn",
      "gender":"F"
    },
    {
      "name":"Adelina",
      "gender":"F"
    },
    {
      "name":"Dinah",
      "gender":"F"
    }    
    ]);

var items = _.shuffle([ 
//    {
//      "trigger":"MC1",
//      "trigger_class":"NonProj"
//    }, 
//    {
//      "trigger":"MC2",
//      "trigger_class":"NonProj"
//    },
//    {
//      "trigger":"MC3",
//      "trigger_class":"NonProj"
//    }, 
//    {
//      "trigger":"MC4",
//      "trigger_class":"NonProj"
//    },
//    {
//      "trigger":"MC5",
//      "trigger_class":"NonProj"
//    },
//    {
//      "trigger":"MC6",
//      "trigger_class":"NonProj"
//    },
//    {
//      "trigger":"MC7",
//      "trigger_class":"NonProj"
//    },
//    {
//      "trigger":"MC8",
//      "trigger_class":"NonProj"
//    },
   {
     "trigger":"be_annoyed",
     "trigger_class":"C"
   }, 
   {
     "trigger":"discover",
     "trigger_class":"C"
   }, 
   {
     "trigger":"know",
     "trigger_class":"C"
   }, 
   {
     "trigger":"reveal",
     "trigger_class":"C"
   },
   {
     "trigger":"see",
     "trigger_class":"C"
   },
   {
     "trigger":"pretend",
     "trigger_class":"C"
   }, 
   {
     "trigger":"suggest",
     "trigger_class":"C"
   }, 
   {
     "trigger":"say",
     "trigger_class":"C"
   }, 
   {
     "trigger":"think",
     "trigger_class":"C"
   },
   {
     "trigger":"be_right",
     "trigger_class":"C"
   },
   {
     "trigger":"demonstrate",
     "trigger_class":"C"
   },
   {
     "trigger":"acknowledge",
     "trigger_class":"C"
   },
   {
     "trigger":"admit",
     "trigger_class":"C"
   },
   {
     "trigger":"announce",
     "trigger_class":"C"
   },
   {
     "trigger":"confess",
     "trigger_class":"C"
   },
   {
     "trigger":"confirm",
     "trigger_class":"C"
   },
   {
     "trigger":"establish",
     "trigger_class":"C"
   },
   {
     "trigger":"hear",
     "trigger_class":"C"
   },
   {
     "trigger":"inform",
     "trigger_class":"C"
   },
   {
     "trigger":"prove",
     "trigger_class":"C"
   }
 ]);

var contents = {
   "mary": {
     "question":"Mary is pregnant",
     "MC":"Mary is pregnant.",
     "be_annoyed":"If Mandy is annoyed that Mary is pregnant, Esther will be mad.",
     "discover":"If Mandy discovered that Mary is pregnant, Esther will be mad.",
     "know":"If Mandy knows that Mary is pregnant, Esther will be mad.",
     "reveal":"If Mandy revealed that Mary is pregnant, Esther will be mad.",
     "see":"If Mandy saw that Mary is pregnant, Esther will be mad.",
     "pretend":"If Mandy pretended that Mary is pregnant, Esther will be mad.",
     "suggest":"If Mandy suggested that Mary is pregnant, Esther will be mad.",
     "say":"If Mandy said that Mary is pregnant, Esther will be mad.",
     "think":"If Mandy thinks that Mary is pregnant, Esther will be mad.",
     "be_right":"If Mandy is right that Mary is pregnant, Esther will be mad.",
     "demonstrate":"If Mandy demonstrated that Mary is pregnant, Esther will be mad.",
     "acknowledge":"If Mandy acknowledged that Mary is pregnant, Esther will be mad.",
     "admit":"If Mandy admitted that Mary is pregnant, Esther will be mad.",
     "announce":"If Mandy announced that Mary is pregnant, Esther will be mad.",
     "confess":"If Mandy confessed that Mary is pregnant, Esther will be mad.",
     "confirm":"If Mandy confirmed that Mary is pregnant, Esther will be mad.",
     "establish":"If Mandy established that Mary is pregnant, Esther will be mad.",
     "hear":"If Mandy heard that Mary is pregnant, Esther will be mad.",
     "inform":"If Mandy informed Sam that Mary is pregnant, Esther will be mad.",
     "prove":"If Mandy proved that Mary is pregnant, Esther will be mad."
   },
   "josie": {
     "question":"Josie went on vacation to France",
     "MC":"Did Josie go on vacation to France.",
     "be_annoyed":"If Sarah is annoyed that Josie went on vacation to France, Arnold will be frustrated.",
     "discover":"If Sarah discovered that Josie went on vacation to France, Arnold will be frustrated.",
     "know":"If Sarah knows that Josie went on vacation to France, Arnold will be frustrated.",
     "reveal":"If Sarah revealed that Josie went on vacation to France, Arnold will be frustrated.",
     "see":"If Sarah saw that Josie went on vacation to France, Arnold will be frustrated.",
     "pretend":"If Sarah pretended that Josie went on vacation to France, Arnold will be frustrated.",
     "suggest":"If Sarah suggested that Josie went on vacation to France, Arnold will be frustrated.",
     "say":"If Sarah said that Josie went on vacation to France, Arnold will be frustrated.",
     "think":"If Sarah thinks that Josie went on vacation to France, Arnold will be frustrated.",
     "be_right":"If Sarah is right that Josie went on vacation to France, Arnold will be frustrated.",
     "demonstrate":"If Sarah demonstrated that Josie went on vacation to France, Arnold will be frustrated.",
     "acknowledge":"If Sarah acknowledged that Josie went on vacation to France, Arnold will be frustrated.",
     "admit":"If Sarah admitted that Josie went on vacation to France, Arnold will be frustrated.",
     "announce":"If Sarah announced that Josie went on vacation to France, Arnold will be frustrated.",
     "confess":"If Sarah confessed  that Josie went on vacation to France, Arnold will be frustrated.",
     "confirm":"If Sarah confirmed that Josie went on vacation to France, Arnold will be frustrated.",
     "establish":"If Sarah established that Josie went on vacation to France, Arnold will be frustrated.",
     "hear":"If Sarah heard that Josie went on vacation to France, Arnold will be frustrated.",
     "inform":"If Sarah informed Sam that Josie went on vacation to France, Arnold will be frustrated.",
     "prove":"If Sarah proved that Josie went on vacation to France, Arnold will be frustrated."
   },
   "emma": {
     "question":"Emma studied on Saturday morning",
     "MC":"Did Emma study on Saturday morning.",
     "be_annoyed":"If Kim is annoyed that Emma studied on Saturday morning, Liam will be proud.",
     "discover":"If Kim discovered that Emma studied on Saturday morning, Liam will be proud.",
     "know":"If Kim knows that Emma studied on Saturday morning, Liam will be proud.",
     "reveal":"If Kim revealed that Emma studied on Saturday morning, Liam will be proud.",
     "see":"If Kim saw that Emma studied on Saturday morning, Liam will be proud.",
     "pretend":"If Kim pretended that Emma studied on Saturday morning, Liam will be proud.",
     "suggest":"If Kim suggested that Emma studied on Saturday morning, Liam will be proud.",
     "say":"If Kim said that Emma studied on Saturday morning, Liam will be proud.",
     "think":"If Kim thinks that Emma studied on Saturday morning, Liam will be proud.",
     "be_right":"If Kim is right that Emma studied on Saturday morning, Liam will be proud.",
     "demonstrate":"If Kim demonstrated that Emma studied on Saturday morning, Liam will be proud.",
     "acknowledge":"If Kim acknowledged that Emma studied on Saturday morning, Liam will be proud.",
     "admit":"If Kim admitted that Emma studied on Saturday morning, Liam will be proud.",
     "announce":"If Kim announced that Emma studied on Saturday morning, Liam will be proud.",
     "confess":"If Kim confessed  that Emma studied on Saturday morning, Liam will be proud.",
     "confirm":"If Kim confirmed that Emma studied on Saturday morning, Liam will be proud.",
     "establish":"If Kim established that Emma studied on Saturday morning, Liam will be proud.",
     "hear":"If Kim heard that Emma studied on Saturday morning, Liam will be proud.",
     "inform":"If Kim informed Sam that Emma studied on Saturday morning, Liam will be proud.",
     "prove":"If Kim proved that Emma studied on Saturday morning, Liam will be proud."
   },
   "olivia": {
     "question":"Olivia sleeps until noon",
     "MC":"Does Olivia sleep until noon.",
     "be_annoyed":"If Jane is annoyed that Olivia sleeps until noon, Elijah will be embarrassed.",
     "discover":"If Jane discovered that Olivia sleeps until noon, Elijah will be embarrassed.",
     "know":"If Jane knows that Olivia sleeps until noon, Elijah will be embarrassed.",
     "reveal":"If Jane revealed that Olivia sleeps until noon, Elijah will be embarrassed.",
     "see":"If Jane saw that Olivia sleeps until noon, Elijah will be embarrassed.",
     "pretend":"If Jane pretended that Olivia sleeps until noon, Elijah will be embarrassed.",
     "suggest":"If Jane suggested that Olivia sleeps until noon, Elijah will be embarrassed.",
     "say":"If Jane said that Olivia sleeps until noon, Elijah will be embarrassed.",
     "think":"If Jane thinks that Olivia sleeps until noon, Elijah will be embarrassed.",
     "be_right":"If Jane is right that Olivia sleeps until noon, Elijah will be embarrassed.",
     "demonstrate":"If Jane demonstrated that Olivia sleeps until noon, Elijah will be embarrassed.",
     "acknowledge":"If Jane acknowledged that Olivia sleeps until noon, Elijah will be embarrassed.",
     "admit":"If Jane admitted that Olivia sleeps until noon, Elijah will be embarrassed.",
     "announce":"If Jane announced that Olivia sleeps until noon, Elijah will be embarrassed.",
     "confess":"If Jane confessed  that Olivia sleeps until noon, Elijah will be embarrassed.",
     "confirm":"If Jane confirmed that Olivia sleeps until noon, Elijah will be embarrassed.",
     "establish":"If Jane established that Olivia sleeps until noon, Elijah will be embarrassed.",
     "hear":"If Jane heard that Olivia sleeps until noon, Elijah will be embarrassed.",
     "inform":"If Jane informed Sam that Olivia sleeps until noon, Elijah will be embarrassed.",
     "prove":"If Jane proved that Olivia sleeps until noon, Elijah will be embarrassed."
   },
   "sophia": {
     "question":"Sophia got a tattoo",
     "MC":"Did Sophia get a tattoo.",
     "be_annoyed":"If Claudia is annoyed that Sophia got a tattoo, Ariel will be giddy.",
     "discover":"If Claudia discovered that Sophia got a tattoo, Ariel will be giddy.",
     "know":"If Claudia knows that Sophia got a tattoo, Ariel will be giddy.",
     "reveal":"If Claudia revealed that Sophia got a tattoo, Ariel will be giddy.",
     "see":"If Claudia saw that Sophia got a tattoo, Ariel will be giddy.",
     "pretend":"If Claudia pretended that Sophia got a tattoo, Ariel will be giddy.",
     "suggest":"If Claudia suggested that Sophia got a tattoo, Ariel will be giddy.",
     "say":"If Claudia said that Sophia got a tattoo, Ariel will be giddy.",
     "think":"If Claudia thinks that Sophia got a tattoo, Ariel will be giddy.",
     "be_right":"If Claudia is right that Sophia got a tattoo, Ariel will be giddy.",
     "demonstrate":"If Claudia demonstrated that Sophia got a tattoo, Ariel will be giddy.",
     "acknowledge":"If Claudia acknowledged that Sophia got a tattoo, Ariel will be giddy.",
     "admit":"If Claudia admitted that Sophia got a tattoo, Ariel will be giddy.",
     "announce":"If Claudia announced that Sophia got a tattoo, Ariel will be giddy.",
     "confess":"If Claudia confessed  that Sophia got a tattoo, Ariel will be giddy.",
     "confirm":"If Claudia confirmed that Sophia got a tattoo, Ariel will be giddy.",
     "establish":"If Claudia established that Sophia got a tattoo, Ariel will be giddy.",
     "hear":"If Claudia heard that Sophia got a tattoo, Ariel will be giddy.",
     "inform":"If Claudia informed Sam that Sophia got a tattoo, Ariel will be giddy.",
     "prove":"If Claudia proved that Sophia got a tattoo, Ariel will be giddy."
   },
   "mia": {
     "question":"Mia drank 2 cocktails last night",
     "MC":"Did Mia drink 2 cocktails last night.",
     "be_annoyed":"If Frank is annoyed that Mia drank 2 cocktails last night, Mariela will be worried.",
     "discover":"If Frank discovered that Mia drank 2 cocktails last night, Mariela will be worried.",
     "know":"If Frank knows that Mia drank 2 cocktails last night, Mariela will be worried.",
     "reveal":"If Frank revealed that Mia drank 2 cocktails last night, Mariela will be worried.",
     "see":"If Frank saw that Mia drank 2 cocktails last night, Mariela will be worried.",
     "pretend":"If Frank pretended that Mia drank 2 cocktails last night, Mariela will be worried.",
     "suggest":"If Frank suggested that Mia drank 2 cocktails last night, Mariela will be worried.",
     "say":"If Frank said that Mia drank 2 cocktails last night, Mariela will be worried.",
     "think":"If Frank thinks that Mia drank 2 cocktails last night, Mariela will be worried.",
     "be_right":"If Frank is right that Mia drank 2 cocktails last night, Mariela will be worried.",
     "demonstrate":"If Frank demonstrated that Mia drank 2 cocktails last night, Mariela will be worried.",
     "acknowledge":"If Frank acknowledged that Mia drank 2 cocktails last night, Mariela will be worried.",
     "admit":"If Frank admitted that Mia drank 2 cocktails last night, Mariela will be worried.",
     "announce":"If Frank announced that Mia drank 2 cocktails last night, Mariela will be worried.",
     "confess":"If Frank confessed  that Mia drank 2 cocktails last night, Mariela will be worried.",
     "confirm":"If Frank confirmed that Mia drank 2 cocktails last night, Mariela will be worried.",
     "establish":"If Frank established that Mia drank 2 cocktails last night, Mariela will be worried.",
     "hear":"If Frank heard that Mia drank 2 cocktails last night, Mariela will be worried.",
     "inform":"If Frank informed Sam that Mia drank 2 cocktails last night, Mariela will be worried.",
     "prove":"If Frank proved that Mia drank 2 cocktails last night, Mariela will be worried."
   },
   "isabella": {
     "question":"Isabella ate a steak on Sunday",
     "MC":"Did Isabella eat a steak on Sunday.",
     "be_annoyed":"If Andrea is annoyed that Isabella ate a steak on Sunday, Liz will be delighted.",
     "discover":"If Andrea discovered that Isabella ate a steak on Sunday, Liz will be delighted.",
     "know":"If Andrea knows that Isabella ate a steak on Sunday, Liz will be delighted.",
     "reveal":"If Andrea revealed that Isabella ate a steak on Sunday, Liz will be delighted.",
     "see":"If Andrea saw that Isabella ate a steak on Sunday, Liz will be delighted.",
     "pretend":"If Andrea pretended that Isabella ate a steak on Sunday, Liz will be delighted.",
     "suggest":"If Andrea suggested that Isabella ate a steak on Sunday, Liz will be delighted.",
     "say":"If Andrea said that Isabella ate a steak on Sunday, Liz will be delighted.",
     "think":"If Andrea thinks that Isabella ate a steak on Sunday, Liz will be delighted.",
     "be_right":"If Andrea is right that Isabella ate a steak on Sunday, Liz will be delighted.",
     "demonstrate":"If Andrea demonstrated that Isabella ate a steak on Sunday, Liz will be delighted.",
     "acknowledge":"If Andrea acknowledged that Isabella ate a steak on Sunday, Liz will be delighted.",
     "admit":"If Andrea admitted that Isabella ate a steak on Sunday, Liz will be delighted.",
     "announce":"If Andrea announced that Isabella ate a steak on Sunday, Liz will be delighted.",
     "confess":"If Andrea confessed  that Isabella ate a steak on Sunday, Liz will be delighted.",
     "confirm":"If Andrea confirmed that Isabella ate a steak on Sunday, Liz will be delighted.",
     "establish":"If Andrea established that Isabella ate a steak on Sunday, Liz will be delighted.",
     "hear":"If Andrea heard that Isabella ate a steak on Sunday, Liz will be delighted.",
     "inform":"If Andrea informed Sam that Isabella ate a steak on Sunday, Liz will be delighted.",
     "prove":"If Andrea proved that Isabella ate a steak on Sunday, Liz will be delighted."
   },
  "emily": {
     "question":"Emily bought a car yesterday",
     "MC":"Did Emily buy a car yesterday.",
     "be_annoyed":"If Chloe is annoyed that Emily bought a car yesterday, Kate will be excited.",
     "discover":"If Chloe discovered that Emily bought a car yesterday, Kate will be excited.",
     "know":"If Chloe knows that Emily bought a car yesterday, Kate will be excited.",
     "reveal":"If Chloe revealed that Emily bought a car yesterday, Kate will be excited.",
     "see":"If Chloe saw that Emily bought a car yesterday, Kate will be excited.",
     "pretend":"If Chloe pretended that Emily bought a car yesterday, Kate will be excited.",
     "suggest":"If Chloe suggested that Emily bought a car yesterday, Kate will be excited.",
     "say":"If Chloe said that Emily bought a car yesterday, Kate will be excited.",
     "think":"If Chloe thinks that Emily bought a car yesterday, Kate will be excited.",
     "be_right":"If Chloe is right that Emily bought a car yesterday, Kate will be excited.",
     "demonstrate":"If Chloe demonstrated that Emily bought a car yesterday, Kate will be excited.",
     "acknowledge":"If Chloe acknowledged that Emily bought a car yesterday, Kate will be excited.",
     "admit":"If Chloe admitted that Emily bought a car yesterday, Kate will be excited.",
     "announce":"If Chloe announced that Emily bought a car yesterday, Kate will be excited.",
     "confess":"If Chloe confessed  that Emily bought a car yesterday, Kate will be excited.",
     "confirm":"If Chloe confirmed that Emily bought a car yesterday, Kate will be excited.",
     "establish":"If Chloe established that Emily bought a car yesterday, Kate will be excited.",
     "hear":"If Chloe heard that Emily bought a car yesterday, Kate will be excited.",
     "inform":"If Chloe informed Sam that Emily bought a car yesterday, Kate will be excited.",
     "prove":"If Chloe proved that Emily bought a car yesterday, Kate will be excited."
   },
   "grace": {
     "question":"Grace visited her sister",
     "MC":"Did Grace visit her sister.",
     "be_annoyed":"If Andrew is annoyed that Grace visited her sister, Henry will be surprised.",
     "discover":"If Andrew discovered that Grace visited her sister, Henry will be surprised.",
     "know":"If Andrew knows that Grace visited her sister, Henry will be surprised.",
     "reveal":"If Andrew revealed that Grace visited her sister, Henry will be surprised.",
     "see":"If Andrew saw that Grace visited her sister, Henry will be surprised.",
     "pretend":"If Andrew pretended that Grace visited her sister, Henry will be surprised.",
     "suggest":"If Andrew suggested that Grace visited her sister, Henry will be surprised.",
     "say":"If Andrew said that Grace visited her sister, Henry will be surprised.",
     "think":"If Andrew thinks that Grace visited her sister, Henry will be surprised.",
     "be_right":"If Andrew is right that Grace visited her sister, Henry will be surprised.",
     "demonstrate":"If Andrew demonstrated that Grace visited her sister, Henry will be surprised.",
     "acknowledge":"If Andrew acknowledged that Grace visited her sister, Henry will be surprised.",
     "admit":"If Andrew admitted that Grace visited her sister, Henry will be surprised.",
     "announce":"If Andrew announced that Grace visited her sister, Henry will be surprised.",
     "confess":"If Andrew confessed  that Grace visited her sister, Henry will be surprised.",
     "confirm":"If Andrew confirmed that Grace visited her sister, Henry will be surprised.",
     "establish":"If Andrew established that Grace visited her sister, Henry will be surprised.",
     "hear":"If Andrew heard that Grace visited her sister, Henry will be surprised.",
     "inform":"If Andrew informed Sam that Grace visited her sister, Henry will be surprised.",
     "prove":"If Andrew proved that Grace visited her sister, Henry will be surprised."
   },
   "zoe": {
     "question":"Zoe calculated the tip",
     "MC":"Did Zoe calculate the tip.",
     "be_annoyed":"If Mark is annoyed that Zoe calculated the tip, Alex will be astonished.",
     "discover":"If Mark discovered that Zoe calculated the tip, Alex will be astonished.",
     "know":"If Mark knows that Zoe calculated the tip, Alex will be astonished.",
     "reveal":"If Mark revealed that Zoe calculated the tip, Alex will be astonished.",
     "see":"If Mark saw that Zoe calculated the tip, Alex will be astonished.",
     "pretend":"If Mark pretended that Zoe calculated the tip, Alex will be astonished.",
     "suggest":"If Mark suggested that Zoe calculated the tip, Alex will be astonished.",
     "say":"If Mark said that Zoe calculated the tip, Alex will be astonished.",
     "think":"If Mark thinks that Zoe calculated the tip, Alex will be astonished.",
     "be_right":"If Mark is right that Zoe calculated the tip, Alex will be astonished.",
     "demonstrate":"If Mark demonstrated that Zoe calculated the tip, Alex will be astonished.",
     "acknowledge":"If Mark acknowledged that Zoe calculated the tip, Alex will be astonished.",
     "admit":"If Mark admitted that Zoe calculated the tip, Alex will be astonished.",
     "announce":"If Mark announced that Zoe calculated the tip, Alex will be astonished.",
     "confess":"If Mark confessed  that Zoe calculated the tip, Alex will be astonished.",
     "confirm":"If Mark confirmed that Zoe calculated the tip, Alex will be astonished.",
     "establish":"If Mark established that Zoe calculated the tip, Alex will be astonished.",
     "hear":"If Mark heard that Zoe calculated the tip, Alex will be astonished.",
     "inform":"If Mark informed Sam that Zoe calculated the tip, Alex will be astonished.",
     "prove":"If Mark proved that Zoe calculated the tip, Alex will be astonished."
   },
  "danny": {
     "question":"Danny ate the last cupcake",
     "MC":"Did Danny eat the last cupcake.",
     "be_annoyed":"If Kathryn is annoyed that Danny ate the last cupcake, Harper will be disgusted.",
     "discover":"If Kathryn discovered that Danny ate the last cupcake, Harper will be disgusted.",
     "know":"If Kathryn knows that Danny ate the last cupcake, Harper will be disgusted.",
     "reveal":"If Kathryn revealed that Danny ate the last cupcake, Harper will be disgusted.",
     "see":"If Kathryn saw that Danny ate the last cupcake, Harper will be disgusted.",
     "pretend":"If Kathryn pretended that Danny ate the last cupcake, Harper will be disgusted.",
     "suggest":"If Kathryn suggested that Danny ate the last cupcake, Harper will be disgusted.",
     "say":"If Kathryn said that Danny ate the last cupcake, Harper will be disgusted.",
     "think":"If Kathryn thinks that Danny ate the last cupcake, Harper will be disgusted.",
     "be_right":"If Kathryn is right that Danny ate the last cupcake, Harper will be disgusted.",
     "demonstrate":"If Kathryn demonstrated that Danny ate the last cupcake, Harper will be disgusted.",
     "acknowledge":"If Kathryn acknowledged that Danny ate the last cupcake, Harper will be disgusted.",
     "admit":"If Kathryn admitted that Danny ate the last cupcake, Harper will be disgusted.",
     "announce":"If Kathryn announced that Danny ate the last cupcake, Harper will be disgusted.",
     "confess":"If Kathryn confessed  that Danny ate the last cupcake, Harper will be disgusted.",
     "confirm":"If Kathryn confirmed that Danny ate the last cupcake, Harper will be disgusted.",
     "establish":"If Kathryn established that Danny ate the last cupcake, Harper will be disgusted.",
     "hear":"If Kathryn heard that Danny ate the last cupcake, Harper will be disgusted.",
     "inform":"If Kathryn informed Sam that Danny ate the last cupcake, Harper will be disgusted.",
     "prove":"If Kathryn proved that Danny ate the last cupcake, Harper will be disgusted."
   },
  "frank": {
     "question":"Frank got a cat",
     "MC":"Frank get a cat.",
     "be_annoyed":"If Walt is annoyed that Frank got a cat, Lucas will be grouchy.",
     "discover":"If Walt discovered that Frank got a cat, Lucas will be grouchy.",
     "know":"If Walt knows that Frank got a cat, Lucas will be grouchy.",
     "reveal":"If Walt revealed that Frank got a cat, Lucas will be grouchy.",
     "see":"If Walt saw that Frank got a cat, Lucas will be grouchy.",
     "pretend":"If Walt pretended that Frank got a cat, Lucas will be grouchy.",
     "suggest":"If Walt suggested that Frank got a cat, Lucas will be grouchy.",
     "say":"If Walt said that Frank got a cat, Lucas will be grouchy.",
     "think":"If Walt thinks that Frank got a cat, Lucas will be grouchy.",
     "be_right":"If Walt is right that Frank got a cat, Lucas will be grouchy.",
     "demonstrate":"If Walt demonstrated that Frank got a cat, Lucas will be grouchy.",
     "acknowledge":"If Walt acknowledged that Frank got a cat, Lucas will be grouchy.",
     "admit":"If Walt admitted that Frank got a cat, Lucas will be grouchy.",
     "announce":"If Walt announced that Frank got a cat, Lucas will be grouchy.",
     "confess":"If Walt confessed  that Frank got a cat, Lucas will be grouchy.",
     "confirm":"If Walt confirmed that Frank got a cat, Lucas will be grouchy.",
     "establish":"If Walt established that Frank got a cat, Lucas will be grouchy.",
     "hear":"If Walt heard that Frank got a cat, Lucas will be grouchy.",
     "inform":"If Walt informed Sam that Frank got a cat, Lucas will be grouchy.",
     "prove":"If Walt proved that Frank got a cat, Lucas will be grouchy."
   },
   "jackson": {
     "question":"Jackson ran 10 miles",
     "MC":"Did Jackson run 10 miles.",
     "be_annoyed":"If Randy is annoyed that Jackson ran 10 miles, Kayla will be cheerful.",
     "discover":"If Randy discovered that Jackson ran 10 miles, Kayla will be cheerful.",
     "know":"If Randy knows that Jackson ran 10 miles, Kayla will be cheerful.",
     "reveal":"If Randy revealed that Jackson ran 10 miles, Kayla will be cheerful.",
     "see":"If Randy saw that Jackson ran 10 miles, Kayla will be cheerful.",
     "pretend":"If Randy pretended that Jackson ran 10 miles, Kayla will be cheerful.",
     "suggest":"If Randy suggested that Jackson ran 10 miles, Kayla will be cheerful.",
     "say":"If Randy said that Jackson ran 10 miles, Kayla will be cheerful.",
     "think":"If Randy thinks that Jackson ran 10 miles, Kayla will be cheerful.",
     "be_right":"If Randy is right that Jackson ran 10 miles, Kayla will be cheerful.",
     "demonstrate":"If Randy demonstrated that Jackson ran 10 miles, Kayla will be cheerful.",
     "acknowledge":"If Randy acknowledged that Jackson ran 10 miles, Kayla will be cheerful.",
     "admit":"If Randy admitted that Jackson ran 10 miles, Kayla will be cheerful.",
     "announce":"If Randy announced that Jackson ran 10 miles, Kayla will be cheerful.",
     "confess":"If Randy confessed  that Jackson ran 10 miles, Kayla will be cheerful.",
     "confirm":"If Randy confirmed that Jackson ran 10 miles, Kayla will be cheerful.",
     "establish":"If Randy established that Jackson ran 10 miles, Kayla will be cheerful.",
     "hear":"If Randy heard that Jackson ran 10 miles, Kayla will be cheerful.",
     "inform":"If Randy informed Sam that Jackson ran 10 miles, Kayla will be cheerful.",
     "prove":"If Randy proved that Jackson ran 10 miles, Kayla will be cheerful."
   },
   "jayden": {
     "question":"Jayden rented a car",
     "MC":"Did Jayden rent a car.",
     "be_annoyed":"If Herbert is annoyed that Jayden rented a car, Brittany will be furious.",
     "discover":"If Herbert discovered that Jayden rented a car, Brittany will be furious.",
     "know":"If Herbert knows that Jayden rented a car, Brittany will be furious.",
     "reveal":"If Herbert revealed that Jayden rented a car, Brittany will be furious.",
     "see":"If Herbert saw that Jayden rented a car, Brittany will be furious.",
     "pretend":"If Herbert pretended that Jayden rented a car, Brittany will be furious.",
     "suggest":"If Herbert suggested that Jayden rented a car, Brittany will be furious.",
     "say":"If Herbert said that Jayden rented a car, Brittany will be furious.",
     "think":"If Herbert thinks that Jayden rented a car, Brittany will be furious.",
     "be_right":"If Herbert is right that Jayden rented a car, Brittany will be furious.",
     "demonstrate":"If Herbert demonstrated that Jayden rented a car, Brittany will be furious.",
     "acknowledge":"If Herbert acknowledged that Jayden rented a car, Brittany will be furious.",
     "admit":"If Herbert admitted that Jayden rented a car, Brittany will be furious.",
     "announce":"If Herbert announced that Jayden rented a car, Brittany will be furious.",
     "confess":"If Herbert confessed  that Jayden rented a car, Brittany will be furious.",
     "confirm":"If Herbert confirmed that Jayden rented a car, Brittany will be furious.",
     "establish":"If Herbert established that Jayden rented a car, Brittany will be furious.",
     "hear":"If Herbert heard that Jayden rented a car, Brittany will be furious.",
     "inform":"If Herbert informed Sam that Jayden rented a car, Brittany will be furious.",
     "prove":"If Herbert proved that Jayden rented a car, Brittany will be furious."
   },
   "tony": {
     "question":"Tony had a drink last night",
     "MC":"Did Tony have a drink last night.",
     "be_annoyed":"If Helen is annoyed that Tony had a drink last night, Victoria will be ashamed.",
     "discover":"If Helen discovered that Tony had a drink last night, Victoria will be ashamed.",
     "know":"If Helen knows that Tony had a drink last night, Victoria will be ashamed.",
     "reveal":"If Helen revealed that Tony had a drink last night, Victoria will be ashamed.",
     "see":"If Helen saw that Tony had a drink last night, Victoria will be ashamed.",
     "pretend":"If Helen pretended that Tony had a drink last night, Victoria will be ashamed.",
     "suggest":"If Helen suggested that Tony had a drink last night, Victoria will be ashamed.",
     "say":"If Helen said that Tony had a drink last night, Victoria will be ashamed.",
     "think":"If Helen thinks that Tony had a drink last night, Victoria will be ashamed.",
     "be_right":"If Helen is right that Tony had a drink last night, Victoria will be ashamed.",
     "demonstrate":"If Helen demonstrated that Tony had a drink last night, Victoria will be ashamed.",
     "acknowledge":"If Helen acknowledged that Tony had a drink last night, Victoria will be ashamed.",
     "admit":"If Helen admitted that Tony had a drink last night, Victoria will be ashamed.",
     "announce":"If Helen announced that Tony had a drink last night, Victoria will be ashamed.",
     "confess":"If Helen confessed  that Tony had a drink last night, Victoria will be ashamed.",
     "confirm":"If Helen confirmed that Tony had a drink last night, Victoria will be ashamed.",
     "establish":"If Helen established that Tony had a drink last night, Victoria will be ashamed.",
     "hear":"If Helen heard that Tony had a drink last night, Victoria will be ashamed.",
     "inform":"If Helen informed Sam that Tony had a drink last night, Victoria will be ashamed.",
     "prove":"If Helen proved that Tony had a drink last night, Victoria will be ashamed."
   },
   "josh": {
     "question":"Josh learned to ride a bike yesterday",
     "MC":"Did Josh learn to ride a bike yesterday.",
     "be_annoyed":"If Brad is annoyed that Josh learned to ride a bike yesterday, Mason will be envious.",
     "discover":"If Brad discovered that Josh learned to ride a bike yesterday, Mason will be envious.",
     "know":"If Brad knows that Josh learned to ride a bike yesterday, Mason will be envious.",
     "reveal":"If Brad revealed that Josh learned to ride a bike yesterday, Mason will be envious.",
     "see":"If Brad saw that Josh learned to ride a bike yesterday, Mason will be envious.",
     "pretend":"If Brad pretended that Josh learned to ride a bike yesterday, Mason will be envious.",
     "suggest":"If Brad suggested that Josh learned to ride a bike yesterday, Mason will be envious.",
     "say":"If Brad said that Josh learned to ride a bike yesterday, Mason will be envious.",
     "think":"If Brad thinks that Josh learned to ride a bike yesterday, Mason will be envious.",
     "be_right":"If Brad is right that Josh learned to ride a bike yesterday, Mason will be envious.",
     "demonstrate":"If Brad demonstrated that Josh learned to ride a bike yesterday, Mason will be envious.",
     "acknowledge":"If Brad acknowledged that Josh learned to ride a bike yesterday, Mason will be envious.",
     "admit":"If Brad admitted that Josh learned to ride a bike yesterday, Mason will be envious.",
     "announce":"If Brad announced that Josh learned to ride a bike yesterday, Mason will be envious.",
     "confess":"If Brad confessed  that Josh learned to ride a bike yesterday, Mason will be envious.",
     "confirm":"If Brad confirmed that Josh learned to ride a bike yesterday, Mason will be envious.",
     "establish":"If Brad established that Josh learned to ride a bike yesterday, Mason will be envious.",
     "hear":"If Brad heard that Josh learned to ride a bike yesterday, Mason will be envious.",
     "inform":"If Brad informed Sam that Josh learned to ride a bike yesterday, Mason will be envious.",
     "prove":"If Brad proved that Josh learned to ride a bike yesterday, Mason will be envious."
   },
   "owen": {
     "question":"Owen shoveled snow last winter",
     "MC":"Did Owen shovel snow last winter.",
     "be_annoyed":"If Jordan is annoyed that Owen shoveled snow last winter, Bianca will be jealous.",
     "discover":"If Jordan discovered that Owen shoveled snow last winter, Bianca will be jealous.",
     "know":"If Jordan knows that Owen shoveled snow last winter, Bianca will be jealous.",
     "reveal":"If Jordan revealed that Owen shoveled snow last winter, Bianca will be jealous.",
     "see":"If Jordan saw that Owen shoveled snow last winter, Bianca will be jealous.",
     "pretend":"If Jordan pretended that Owen shoveled snow last winter, Bianca will be jealous.",
     "suggest":"If Jordan suggested that Owen shoveled snow last winter, Bianca will be jealous.",
     "say":"If Jordan said that Owen shoveled snow last winter, Bianca will be jealous.",
     "think":"If Jordan thinks that Owen shoveled snow last winter, Bianca will be jealous.",
     "be_right":"If Jordan is right that Owen shoveled snow last winter, Bianca will be jealous.",
     "demonstrate":"If Jordan demonstrated that Owen shoveled snow last winter, Bianca will be jealous.",
     "acknowledge":"If Jordan acknowledged that Owen shoveled snow last winter, Bianca will be jealous.",
     "admit":"If Jordan admitted that Owen shoveled snow last winter, Bianca will be jealous.",
     "announce":"If Jordan announced that Owen shoveled snow last winter, Bianca will be jealous.",
     "confess":"If Jordan confessed  that Owen shoveled snow last winter, Bianca will be jealous.",
     "confirm":"If Jordan confirmed that Owen shoveled snow last winter, Bianca will be jealous.",
     "establish":"If Jordan established that Owen shoveled snow last winter, Bianca will be jealous.",
     "hear":"If Jordan heard that Owen shoveled snow last winter, Bianca will be jealous.",
     "inform":"If Jordan informed Sam that Owen shoveled snow last winter, Bianca will be jealous.",
     "prove":"If Jordan proved that Owen shoveled snow last winter, Bianca will be jealous."
   },
   "julian": {
     "question":"Julian dances salsa",
     "MC":"Does Julian dance salsa.",
     "be_annoyed":"If Cole is annoyed that Julian dances salsa, Logan will be joyful.",
     "discover":"If Cole discovered that Julian dances salsa, Logan will be joyful.",
     "know":"If Cole knows that Julian dances salsa, Logan will be joyful.",
     "reveal":"If Cole revealed that Julian dances salsa, Logan will be joyful.",
     "see":"If Cole saw that Julian dances salsa, Logan will be joyful.",
     "pretend":"If Cole pretended that Julian dances salsa, Logan will be joyful.",
     "suggest":"If Cole suggested that Julian dances salsa, Logan will be joyful.",
     "say":"If Cole said that Julian dances salsa, Logan will be joyful.",
     "think":"If Cole thinks that Julian dances salsa, Logan will be joyful.",
     "be_right":"If Cole is right that Julian dances salsa, Logan will be joyful.",
     "demonstrate":"If Cole demonstrated that Julian dances salsa, Logan will be joyful.",
     "acknowledge":"If Cole acknowledged that Julian dances salsa, Logan will be joyful.",
     "admit":"If Cole admitted that Julian dances salsa, Logan will be joyful.",
     "announce":"If Cole announced that Julian dances salsa, Logan will be joyful.",
     "confess":"If Cole confessed  that Julian dances salsa, Logan will be joyful.",
     "confirm":"If Cole confirmed that Julian dances salsa, Logan will be joyful.",
     "establish":"If Cole established that Julian dances salsa, Logan will be joyful.",
     "hear":"If Cole heard that Julian dances salsa, Logan will be joyful.",
     "inform":"If Cole informed Sam that Julian dances salsa, Logan will be joyful.",
     "prove":"If Cole proved that Julian dances salsa, Logan will be joyful."
   },
   "jon": {
     "question":"Jon walks to work",
     "MC":"Does Jon walk to work.",
     "be_annoyed":"If Dexter is annoyed that Jon walks to work, Caleb will be suspicious.",
     "discover":"If Dexter discovered that Jon walks to work, Caleb will be suspicious.",
     "know":"If Dexter knows that Jon walks to work, Caleb will be suspicious.",
     "reveal":"If Dexter revealed that Jon walks to work, Caleb will be suspicious.",
     "see":"If Dexter saw that Jon walks to work, Caleb will be suspicious.",
     "pretend":"If Dexter pretended that Jon walks to work, Caleb will be suspicious.",
     "suggest":"If Dexter suggested that Jon walks to work, Caleb will be suspicious.",
     "say":"If Dexter said that Jon walks to work, Caleb will be suspicious.",
     "think":"If Dexter thinks that Jon walks to work, Caleb will be suspicious.",
     "be_right":"If Dexter is right that Jon walks to work, Caleb will be suspicious.",
     "demonstrate":"If Dexter demonstrated that Jon walks to work, Caleb will be suspicious.",
     "acknowledge":"If Dexter acknowledged that Jon walks to work, Caleb will be suspicious.",
     "admit":"If Dexter admitted that Jon walks to work, Caleb will be suspicious.",
     "announce":"If Dexter announced that Jon walks to work, Caleb will be suspicious.",
     "confess":"If Dexter confessed  that Jon walks to work, Caleb will be suspicious.",
     "confirm":"If Dexter confirmed that Jon walks to work, Caleb will be suspicious.",
     "establish":"If Dexter established that Jon walks to work, Caleb will be suspicious.",
     "hear":"If Dexter heard that Jon walks to work, Caleb will be suspicious.",
     "inform":"If Dexter informed Sam that Jon walks to work, Caleb will be suspicious.",
     "prove":"If Dexter proved that Jon walks to work, Caleb will be suspicious."
   },
   "charley": {
     "question":"Charley speaks Spanish",
     "MC":"Does Charley speak Spanish.",
     "be_annoyed":"If Anton is annoyed that Charley speaks Spanish, Jay will be happy.",
     "discover":"If Anton discovered that Charley speaks Spanish, Jay will be happy.",
     "know":"If Anton knows that Charley speaks Spanish, Jay will be happy.",
     "reveal":"If Anton revealed that Charley speaks Spanish, Jay will be happy.",
     "see":"If Anton saw that Charley speaks Spanish, Jay will be happy.",
     "pretend":"If Anton pretended that Charley speaks Spanish, Jay will be happy.",
     "suggest":"If Anton suggested that Charley speaks Spanish, Jay will be happy.",
     "say":"If Anton said that Charley speaks Spanish, Jay will be happy.",
     "think":"If Anton thinks that Charley speaks Spanish, Jay will be happy.",
     "be_right":"If Anton is right that Charley speaks Spanish, Jay will be happy.",
     "demonstrate":"If Anton demonstrated that Charley speaks Spanish, Jay will be happy.",
     "acknowledge":"If Anton acknowledged that Charley speaks Spanish, Jay will be happy.",
     "admit":"If Anton admitted that Charley speaks Spanish, Jay will be happy.",
     "announce":"If Anton announced that Charley speaks Spanish, Jay will be happy.",
     "confess":"If Anton confessed  that Charley speaks Spanish, Jay will be happy.",
     "confirm":"If Anton confirmed that Charley speaks Spanish, Jay will be happy.",
     "establish":"If Anton established that Charley speaks Spanish, Jay will be happy.",
     "hear":"If Anton heard that Charley speaks Spanish, Jay will be happy.",
     "inform":"If Anton informed Sam that Charley speaks Spanish, Jay will be happy.",
     "prove":"If Anton proved that Charley speaks Spanish, Jay will be happy."
   }
 };
 
var items_content_mapping = {
"be_annoyed":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"discover":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"know":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"reveal":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"see":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"pretend":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"suggest":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"say":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"think":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"be_right":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"demonstrate":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"acknowledge":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"admit":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"announce":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"confess":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"confirm":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"establish":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"hear":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"inform":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"],
"prove":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"]
//"MC":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley"]
};  

//controls
var mcitemnames = ["muffins","pizza","kids","ballet","garage","hat"];


//var utterance = mcitems[j].MCq;
//    var question = mcitems[j].question;  

var mcitems = {
  "muffins": {
    "question":"these muffins have blueberries in them",
    "MCq":"These muffins have blueberries in them.",
    //"MCa":"These muffins don't have blueberries in them."
    "MCc":"These muffins, which are really delicious, have blueberries in them.",
},
  "pizza": {
    "question":"this pizza has mushrooms on it",
    "MCq":"This pizza has mushrooms on it.",
    //"MCa":"This pizza doesn't have mushrooms on it."
    "MCc":"This pizza, which I just made from scratch, has mushrooms on it.",
  },
  "kids": {
    "question":"Jack was playing outside with the kids",
    "MCq":"Jack was playing outside with the kids.",
    //"MCa":"Jack wasn't playing outside with the kids."
    "MCc":"Jack, who is my long-time neighbor, was playing outside with the kids."},
"ballet": {
    "question":"Ann dances ballet",
    "MCq":"Ann dances ballet.",
    //"MCa":"Ann doesn't dance ballet."
     "MCc": "Ann, who is a local performer, dances ballet."},
"garage": {
    "question":"John's kids were in the garage",
    "MCq":"John's kids were in the garage.",
    //"MCa":"John's kids weren't in the garage."
    "MCc":"John's kids, who are very well-behaved, were in the garage."},
"hat": {
    "question":"Samantha has a new hat",
    "MCq":"Samantha has a new hat.",
    //"MCa":"Samantha doesn't have a new hat."
    "MCc":"Samantha, who is really into fashion, has a new hat.",
    }
};


// get trigger contents
  function getContent(trigger) {
//  		console.log("items_content_mapping before throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}  		
//  		console.log("items_content_mapping at the trigger before shuffling");
//  		console.log(items_content_mapping[trigger]);  		
  		items_content_mapping[trigger] = _.shuffle(items_content_mapping[trigger]);
//  		console.log("items_content_mapping at the trigger after shuffling");
//  		console.log(items_content_mapping[trigger]);  		  		
//  		console.log("items_content_mapping after shuffling "+trigger);
//  		console.log(items_content_mapping);
  		var content = items_content_mapping[trigger].shift();//items_content_mapping[trigger][0];
  		// console.log("this is the selected content: " + content);
//		var index = items_content_mapping[trigger].indexOf(content);  		
//  		items_content_mapping[trigger] = items_content_mapping[trigger].splice(index,1);
//  		console.log("items_content_mapping at the trigger after throwing it out");
//  		console.log(items_content_mapping[trigger]);  		  		
  		for (var j in items_content_mapping) {
			var index = items_content_mapping[j].indexOf(content);  
			// console.log("the next three lines: the array before removal, the index of content, the array after removal")
			// console.log(items_content_mapping[j]);
			// console.log(index);		
			if (index != -1)
			{			  			
				items_content_mapping[j].splice(index,1);			
			}
			// console.log(items_content_mapping[j]);			
			}
//  		console.log("items_content_mapping after throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}   		  		

  		return content;
  	}
  	  
// assign contents to triggers
  var trigger_contents = {
  	"be_annoyed": getContent("be_annoyed"),  	  	
  	"discover": getContent("discover"),
  	"know": getContent("know"),  	  	
  	"reveal": getContent("reveal"),
  	"see": getContent("see"),
  	"pretend": getContent("pretend"),
  	"suggest": getContent("suggest"),  	
  	"say": getContent("say"),  	
  	"think": getContent("think"),
  	"be_right": getContent("be_right"),
  	"demonstrate": getContent("demonstrate"),
  	"acknowledge": getContent("acknowledge"),
  	"admit": getContent("admit"),
  	"announce": getContent("announce"),
  	"confess": getContent("confess"),
  	"confirm": getContent("confirm"),
  	"establish": getContent("establish"),
  	"hear": getContent("hear"),
  	"inform": getContent("inform"),
  	"prove": getContent("prove")
//   	"MC1": getContent("MC"),
//   	"MC2": getContent("MC"),  	
//   	"MC3": getContent("MC"),
//   	"MC4": getContent("MC"),
//   	"MC5": getContent("MC")
  	};
       
  function makeStim(i,prior) {
    //get item
    var item = items[i];
	//get a speaker
    var name_data = names[i];
    var name = name_data.name;
    var gender = name_data.gender;
    //get another speaker
    var name_data2 = names[i+20];
    var name2 = name_data2.name;
    var gender2 = name_data2.gender;
    // get content
    var trigger_cont = trigger_contents[item.trigger];
    var trigger = item.trigger;
    var short_trigger = trigger;
    if (trigger.indexOf("MC") != -1) {
    	short_trigger = "MC";
    	}
//  console.log("short_trigger: "+short_trigger);
//	console.log("trigger: "+trigger);
    console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = contents[trigger_cont][short_trigger];
    var question = contents[trigger_cont].question; 
    var prior  
//    console.log(contents[trigger_cont]); 
    return {
	  "name": name,
	  "gender": gender,	
	  "name2": name2,
	  "gender2": gender2,  
	  "trigger": item.trigger,
	  "short_trigger": short_trigger,	  
	  "trigger_class": item.trigger_class,
      "content": trigger_cont,
      "utterance": utterance,
      "question": question
    }
  }
  
// item from items::
//      {
//      "trigger":"be_annoyed",
//      "trigger_class":"C"
//    }, 

// trigger_content from trigger_contents::
// "be_annoyed": getContent("be_annoyed"), 
// -->"mary"

// content from contents::
//    "mary": {
//      "question":"Mary is pregnant",
//      "MC":"Is Mary pregnant?",
//      "be_annoyed":"Mandy isn't annoyed that Mary is pregnant?",
//      "discover":"Mandy didn't discover that Mary is pregnant?",
//      "know":"Mandy doesn't know that Mary is pregnant?",
//      "reveal":"Mandy didn't reveal that Mary is pregnant?",
//      "see":"Mandy didn't see that Mary is pregnant?",
//      "pretend":"Mandy didn't pretend that Mary is pregnant?",
//      "suggest":"Mandy didn't suggest that Mary is pregnant?",
//      "say":"Mandy didn't say that Mary is pregnant?",
//      "think":"Mandy doesn't think that Mary is pregnant?",
//      "be_right":"Mandy isn't right that Mary is pregnant?",
//      "demonstrate":"Mandy didn't demonstrate that Mary is pregnant?",
//      "acknowledge":"Mandy didn't acknowledge that Mary is pregnant?",
//      "admit":"Mandy didn't admit that Mary is pregnant?",
//      "announce":"Mandy didn't announce that Mary is pregnant?",
//      "confess":"Mandy didn't confess that Mary is pregnant?",
//      "confirm":"Mandy didn't confirm that Mary is pregnant?",
//      "establish":"Mandy didn't establish that Mary is pregnant?",
//      "hear":"Mandy didn't hear that Mary is pregnant?",
//      "inform":"Mandy didn't inform Sam that Mary is pregnant?",
//      "prove":"Mandy didn't prove that Mary is pregnant?"
//    },

  function makeMCStim(ind,j) {
    //get item
    var item = mcitems[j];
  //get a speaker
    var name_data = mcnames[ind];
    var name = name_data.name;
    var gender = name_data.gender;
    //get a speaker
    var name_data2 = mcnames[ind+6];
    var name2 = name_data2.name;
    var gender2 = name_data2.gender;
    // get content
    var trigger_cont = j;
    var trigger = "MC";
    var short_trigger = "MC";

//  console.log("short_trigger: "+short_trigger);
//  console.log("trigger: "+trigger);
    console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = mcitems[j].MCc;
    var question = mcitems[j].question;   
//    console.log(contents[trigger_cont]); 
    return {
    "name": name,
    "gender": gender, 
    "name2": name2,
    "gender2": gender2,  
    "trigger": trigger,
    "short_trigger": short_trigger,   
    "trigger_class": "MC",
      "content": trigger_cont,
      "utterance": utterance,
      "question": question
    }
  }  

exp.stims_block1 = [];
exp.stims_block2 = [];

  for (var i=0; i<items.length/2; i++) {
  	var stim = makeStim(i,"low_prior");
  	exp.stims_block1.push(jQuery.extend(true, {}, stim));
	  exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }
  
  for (var i=items.length/2; i<items.length; i++) {
  	var stim = makeStim(i,"high_prior");
  	exp.stims_block1.push(jQuery.extend(true, {}, stim));
	  exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }    

  for (var j=0; j<mcitemnames.length; j++) {
    var stim = makeMCStim(j,mcitemnames[j]);
    exp.stims_block1.push(jQuery.extend(true, {}, stim));
    exp.stims_block2.push(jQuery.extend(true, {}, stim)); 
  }  
  
console.log(exp.stims_block1);
console.log(exp.stims_block2);   

	exp.stims_block1 = _.shuffle(exp.stims_block1);  
	exp.stims_block2 = _.shuffle(exp.stims_block2); 
	
// decide which block comes first
  var block_order = _.shuffle(["ai","projective"]);
  var block1type = block_order[0];
  var block2type = block_order[1];  
  // console.log(block_order);
  // console.log(block1type);  
  // console.log(block2type);
  // console.log(block1type);    

   for (var k=0; k<exp.stims_block2.length; k++) {    
   		exp.stims_block2[k].block = block2type;//block_order[1];
      // console.log(exp.stims_block2[k].block);   	
   		exp.stims_block1[k].block = block1type;//block_order[0];   	
      // console.log(exp.stims_block1[k].block);
   	}


console.log(exp.stims_block1);
console.log(exp.stims_block2);   	

//  exp.all_stims = [];
//  for (var i=0; i<items.length; i++) {
//    exp.all_stims.push(makeStim(i));
//  }
//
//	for (k in exp.all_stims) {
//		console.log(exp.all_stims[k].content)
//		}

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["botcaptcha", "i0", "instructions", "instructions1", "block1", "instructions2", "block2", 'questionaire', 'finished'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 3 + 26 + 1 + 26 + 1; 
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    //if (turk.previewMode) {
     // $("#mustaccept").show();
    //} else {
    //  $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    //}
  });

  exp.go(); //show first slide
}