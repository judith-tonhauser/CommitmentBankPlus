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
    		inst1 = inst1 + "First you'll answer questions about whether one person answered the other person's question."
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
	  		utterance = "<table><tr><td><strong>"+this.stim.name + ":</strong> \"<i>" +this.stim.utterance+"</i>\"</td></tr><tr><td><strong>"+this.stim.name2 + ":</strong> \"<i>Are you sure?</i>\""+"</td></tr><tr><td><strong>"+this.stim.name + ": </strong> \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""+"</td></tr></table>";
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
	  		question = "Did "+this.stim.name+" answer "+this.stim.name2+"'s question?";
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
    		inst2 = inst2 + "Now you'll answer questions about whether one person answered the other person's question."
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
	  		utterance = "<table><tr><td><strong>"+this.stim.name + ":</strong> \"<i>" +this.stim.utterance+"</i>\"</td></tr><tr><td><strong>"+this.stim.name2 + ":</strong> \"<i>Are you sure?</i>\""+"</td></tr><tr><td><strong>"+this.stim.name + ": </strong> \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""+"</td></tr></table>";
	  } else {
	  		utterance = "<strong>"+this.stim.name+": </strong>\"<i>"+this.stim.utterance+"</i>\"";	  	
	  	}      
	  $(".sentence").html(utterance);
	  var question = "";
	  console.log(this.stim.block);	  
	  if (this.stim.block == "ai") {
	  		question = "Did "+this.stim.name+" answer "+this.stim.name2+"'s question?";
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
     "be_annoyed":"Mandy isn't annoyed that Mary is pregnant.",
     "discover":"Mandy didn't discover that Mary is pregnant.",
     "know":"Mandy doesn't know that Mary is pregnant.",
     "reveal":"Mandy didn't reveal that Mary is pregnant.",
     "see":"Mandy didn't see that Mary is pregnant.",
     "pretend":"Mandy didn't pretend that Mary is pregnant.",
     "suggest":"Mandy didn't suggest that Mary is pregnant.",
     "say":"Mandy didn't say that Mary is pregnant.",
     "think":"Mandy doesn't think that Mary is pregnant.",
     "be_right":"Mandy isn't right that Mary is pregnant.",
     "demonstrate":"Mandy didn't demonstrate that Mary is pregnant.",
     "acknowledge":"Mandy didn't acknowledge that Mary is pregnant.",
     "admit":"Mandy didn't admit that Mary is pregnant.",
     "announce":"Mandy didn't announce that Mary is pregnant.",
     "confess":"Mandy didn't confess that Mary is pregnant.",
     "confirm":"Mandy didn't confirm that Mary is pregnant.",
     "establish":"Mandy didn't establish that Mary is pregnant.",
     "hear":"Mandy didn't hear that Mary is pregnant.",
     "inform":"Mandy didn't inform Sam that Mary is pregnant.",
     "prove":"Mandy didn't prove that Mary is pregnant."
   },
   "josie": {
     "question":"Josie went on vacation to France",
     "MC":"Did Josie go on vacation to France.",
     "be_annoyed":"Sarah isn't annoyed that Josie went on vacation to France.",
     "discover":"Sarah didn't discover that Josie went on vacation to France.",
     "know":"Sarah doesn't know that Josie went on vacation to France.",
     "reveal":"Sarah didn't reveal that Josie went on vacation to France.",
     "see":"Sarah didn't see that Josie went on vacation to France.",
     "pretend":"Sarah didn't pretend that Josie went on vacation to France.",
     "suggest":"Sarah didn't suggest that Josie went on vacation to France.",
     "say":"Sarah didn't say that Josie went on vacation to France.",
     "think":"Sarah doesn't think that Josie went on vacation to France.",
     "be_right":"Sarah isn't right that Josie went on vacation to France.",
     "demonstrate":"Sarah didn't demonstrate that Josie went on vacation to France.",
     "acknowledge":"Sarah didn't acknowledge that Josie went on vacation to France.",
     "admit":"Sarah didn't admit that Josie went on vacation to France.",
     "announce":"Sarah didn't announce that Josie went on vacation to France.",
     "confess":"Sarah didn't confess that Josie went on vacation to France.",
     "confirm":"Sarah didn't confirm that Josie went on vacation to France.",
     "establish":"Sarah didn't establish that Josie went on vacation to France.",
     "hear":"Sarah didn't hear that Josie went on vacation to France.",
     "inform":"Sarah didn't inform Sam that Josie went on vacation to France.",
     "prove":"Sarah didn't prove that Josie went on vacation to France."
   },
   "emma": {
     "question":"Emma studied on Saturday morning",
     "MC":"Did Emma study on Saturday morning.",
     "be_annoyed":"Kim isn't annoyed that Emma studied on Saturday morning.",
     "discover":"Kim didn't discover that Emma studied on Saturday morning.",
     "know":"Kim doesn't know that Emma studied on Saturday morning.",
     "reveal":"Kim didn't reveal that Emma studied on Saturday morning.",
     "see":"Kim didn't see that Emma studied on Saturday morning.",
     "pretend":"Kim didn't pretend that Emma studied on Saturday morning.",
     "suggest":"Kim didn't suggest that Emma studied on Saturday morning.",
     "say":"Kim didn't say that Emma studied on Saturday morning.",
     "think":"Kim doesn't think that Emma studied on Saturday morning.",
     "be_right":"Kim isn't right that Emma studied on Saturday morning.",
     "demonstrate":"Kim didn't demonstrate that Emma studied on Saturday morning.",
     "acknowledge":"Kim didn't acknowledge that Emma studied on Saturday morning.",
     "admit":"Kim didn't admit that Emma studied on Saturday morning.",
     "announce":"Kim didn't announce that Emma studied on Saturday morning.",
     "confess":"Kim didn't confess that Emma studied on Saturday morning.",
     "confirm":"Kim didn't confirm that Emma studied on Saturday morning.",
     "establish":"Kim didn't establish that Emma studied on Saturday morning.",
     "hear":"Kim didn't hear that Emma studied on Saturday morning.",
     "inform":"Kim didn't inform Sam that Emma studied on Saturday morning.",
     "prove":"Kim didn't prove that Emma studied on Saturday morning."
   },
   "olivia": {
     "question":"Olivia sleeps until noon",
     "MC":"Does Olivia sleep until noon.",
     "be_annoyed":"Jane isn't annoyed that Olivia sleeps until noon.",
     "discover":"Jane didn't discover that Olivia sleeps until noon.",
     "know":"Jane doesn't know that Olivia sleeps until noon.",
     "reveal":"Jane didn't reveal that Olivia sleeps until noon.",
     "see":"Jane didn't see that Olivia sleeps until noon.",
     "pretend":"Jane didn't pretend that Olivia sleeps until noon.",
     "suggest":"Jane didn't suggest that Olivia sleeps until noon.",
     "say":"Jane didn't say that Olivia sleeps until noon.",
     "think":"Jane doesn't think that Olivia sleeps until noon.",
     "be_right":"Jane isn't right that Olivia sleeps until noon.",
     "demonstrate":"Jane didn't demonstrate that Olivia sleeps until noon.",
     "acknowledge":"Jane didn't acknowledge that Olivia sleeps until noon.",
     "admit":"Jane didn't admit that Olivia sleeps until noon.",
     "announce":"Jane didn't announce that Olivia sleeps until noon.",
     "confess":"Jane didn't confess that Olivia sleeps until noon.",
     "confirm":"Jane didn't confirm that Olivia sleeps until noon.",
     "establish":"Jane didn't establish that Olivia sleeps until noon.",
     "hear":"Jane didn't hear that Olivia sleeps until noon.",
     "inform":"Jane didn't inform Sam that Olivia sleeps until noon.",
     "prove":"Jane didn't prove that Olivia sleeps until noon."
   },
   "sophia": {
     "question":"Sophia got a tattoo",
     "MC":"Did Sophia get a tattoo.",
     "be_annoyed":"Claudia isn't annoyed that Sophia got a tattoo.",
     "discover":"Claudia didn't discover that Sophia got a tattoo.",
     "know":"Claudia doesn't know that Sophia got a tattoo.",
     "reveal":"Claudia didn't reveal that Sophia got a tattoo.",
     "see":"Claudia didn't see that Sophia got a tattoo.",
     "pretend":"Claudia didn't pretend that Sophia got a tattoo.",
     "suggest":"Claudia didn't suggest that Sophia got a tattoo.",
     "say":"Claudia didn't say that Sophia got a tattoo.",
     "think":"Claudia doesn't think that Sophia got a tattoo.",
     "be_right":"Claudia isn't right that Sophia got a tattoo.",
     "demonstrate":"Claudia didn't demonstrate that Sophia got a tattoo.",
     "acknowledge":"Claudia didn't acknowledge that Sophia got a tattoo.",
     "admit":"Claudia didn't admit that Sophia got a tattoo.",
     "announce":"Claudia didn't announce that Sophia got a tattoo.",
     "confess":"Claudia didn't confess that Sophia got a tattoo.",
     "confirm":"Claudia didn't confirm that Sophia got a tattoo.",
     "establish":"Claudia didn't establish that Sophia got a tattoo.",
     "hear":"Claudia didn't hear that Sophia got a tattoo.",
     "inform":"Claudia didn't inform Sam that Sophia got a tattoo.",
     "prove":"Claudia didn't prove that Sophia got a tattoo."
   },
   "mia": {
     "question":"Mia drank 2 cocktails last night",
     "MC":"Did Mia drink 2 cocktails last night.",
     "be_annoyed":"Frank isn't annoyed that Mia drank 2 cocktails last night.",
     "discover":"Frank didn't discover that Mia drank 2 cocktails last night.",
     "know":"Frank doesn't know that Mia drank 2 cocktails last night.",
     "reveal":"Frank didn't reveal that Mia drank 2 cocktails last night.",
     "see":"Frank didn't see that Mia drank 2 cocktails last night.",
     "pretend":"Frank didn't pretend that Mia drank 2 cocktails last night.",
     "suggest":"Frank didn't suggest that Mia drank 2 cocktails last night.",
     "say":"Frank didn't say that Mia drank 2 cocktails last night.",
     "think":"Frank doesn't think that Mia drank 2 cocktails last night.",
     "be_right":"Frank isn't right that Mia drank 2 cocktails last night.",
     "demonstrate":"Frank didn't demonstrate that Mia drank 2 cocktails last night.",
     "acknowledge":"Frank didn't acknowledge that Mia drank 2 cocktails last night.",
     "admit":"Frank didn't admit that Mia drank 2 cocktails last night.",
     "announce":"Frank didn't announce that Mia drank 2 cocktails last night.",
     "confess":"Frank didn't confess that Mia drank 2 cocktails last night.",
     "confirm":"Frank didn't confirm that Mia drank 2 cocktails last night.",
     "establish":"Frank didn't establish that Mia drank 2 cocktails last night.",
     "hear":"Frank didn't hear that Mia drank 2 cocktails last night.",
     "inform":"Frank didn't inform Sam that Mia drank 2 cocktails last night.",
     "prove":"Frank didn't prove that Mia drank 2 cocktails last night."
   },
   "isabella": {
     "question":"Isabella ate a steak on Sunday",
     "MC":"Did Isabella eat a steak on Sunday.",
     "be_annoyed":"Andrea isn't annoyed that Isabella ate a steak on Sunday.",
     "discover":"Andrea didn't discover that Isabella ate a steak on Sunday.",
     "know":"Andrea doesn't know that Isabella ate a steak on Sunday.",
     "reveal":"Andrea didn't reveal that Isabella ate a steak on Sunday.",
     "see":"Andrea didn't see that Isabella ate a steak on Sunday.",
     "pretend":"Andrea didn't pretend that Isabella ate a steak on Sunday.",
     "suggest":"Andrea didn't suggest that Isabella ate a steak on Sunday.",
     "say":"Andrea didn't say that Isabella ate a steak on Sunday.",
     "think":"Andrea doesn't think that Isabella ate a steak on Sunday.",
     "be_right":"Andrea isn't right that Isabella ate a steak on Sunday.",
     "demonstrate":"Andrea didn't demonstrate that Isabella ate a steak on Sunday.",
     "acknowledge":"Andrea didn't acknowledge that Isabella ate a steak on Sunday.",
     "admit":"Andrea didn't admit that Isabella ate a steak on Sunday.",
     "announce":"Andrea didn't announce that Isabella ate a steak on Sunday.",
     "confess":"Andrea didn't confess that Isabella ate a steak on Sunday.",
     "confirm":"Andrea didn't confirm that Isabella ate a steak on Sunday.",
     "establish":"Andrea didn't establish that Isabella ate a steak on Sunday.",
     "hear":"Andrea didn't hear that Isabella ate a steak on Sunday.",
     "inform":"Andrea didn't inform Sam that Isabella ate a steak on Sunday.",
     "prove":"Andrea didn't prove that Isabella ate a steak on Sunday."
   },
   "emily": {
     "question":"Emily bought a car yesterday",
     "MC":"Did Emily buy a car yesterday.",
     "be_annoyed":"Chloe isn't annoyed that Emily bought a car yesterday.",
     "discover":"Chloe didn't discover that Emily bought a car yesterday.",
     "know":"Chloe doesn't know that Emily bought a car yesterday.",
     "reveal":"Chloe didn't reveal that Emily bought a car yesterday.",
     "see":"Chloe didn't see that Emily bought a car yesterday.",
     "pretend":"Chloe didn't pretend that Emily bought a car yesterday.",
     "suggest":"Chloe didn't suggest that Emily bought a car yesterday.",
     "say":"Chloe didn't say that Emily bought a car yesterday.",
     "think":"Chloe doesn't think that Emily bought a car yesterday.",
     "be_right":"Chloe isn't right that Emily bought a car yesterday.",
     "demonstrate":"Chloe didn't demonstrate that Emily bought a car yesterday.",
     "acknowledge":"Chloe didn't acknowledge that Emily bought a car yesterday.",
     "admit":"Chloe didn't admit that Emily bought a car yesterday.",
     "announce":"Chloe didn't announce that Emily bought a car yesterday.",
     "confess":"Chloe didn't confess that Emily bought a car yesterday.",
     "confirm":"Chloe didn't confirm that Emily bought a car yesterday.",
     "establish":"Chloe didn't establish that Emily bought a car yesterday.",
     "hear":"Chloe didn't hear that Emily bought a car yesterday.",
     "inform":"Chloe didn't inform Sam that Emily bought a car yesterday.",
     "prove":"Chloe didn't prove that Emily bought a car yesterday."
   },
   "grace": {
     "question":"Grace visited her sister",
     "MC":"Did Grace visit her sister.",
     "be_annoyed":"Andrew isn't annoyed that Grace visited her sister.",
     "discover":"Andrew didn't discover that Grace visited her sister.",
     "know":"Andrew doesn't know that Grace visited her sister.",
     "reveal":"Andrew didn't reveal that Grace visited her sister.",
     "see":"Andrew didn't see that Grace visited her sister.",
     "pretend":"Andrew didn't pretend that Grace visited her sister.",
     "suggest":"Andrew didn't suggest that Grace visited her sister.",
     "say":"Andrew didn't say that Grace visited her sister.",
     "think":"Andrew doesn't think that Grace visited her sister.",
     "be_right":"Andrew isn't right that Grace visited her sister.",
     "demonstrate":"Andrew didn't demonstrate that Grace visited her sister.",
     "acknowledge":"Andrew didn't acknowledge that Grace visited her sister.",
     "admit":"Andrew didn't admit that Grace visited her sister.",
     "announce":"Andrew didn't announce that Grace visited her sister.",
     "confess":"Andrew didn't confess that Grace visited her sister.",
     "confirm":"Andrew didn't confirm that Grace visited her sister.",
     "establish":"Andrew didn't establish that Grace visited her sister.",
     "hear":"Andrew didn't hear that Grace visited her sister.",
     "inform":"Andrew didn't inform Sam that Grace visited her sister.",
     "prove":"Andrew didn't prove that Grace visited her sister."
   },
   "zoe": {
     "question":"Zoe calculated the tip",
     "MC":"Did Zoe calculate the tip.",
     "be_annoyed":"Mark isn't annoyed that Zoe calculated the tip.",
     "discover":"Mark didn't discover that Zoe calculated the tip.",
     "know":"Mark doesn't know that Zoe calculated the tip.",
     "reveal":"Mark didn't reveal that Zoe calculated the tip.",
     "see":"Mark didn't see that Zoe calculated the tip.",
     "pretend":"Mark didn't pretend that Zoe calculated the tip.",
     "suggest":"Mark didn't suggest that Zoe calculated the tip.",
     "say":"Mark didn't say that Zoe calculated the tip.",
     "think":"Mark doesn't think that Zoe calculated the tip.",
     "be_right":"Mark isn't right that Zoe calculated the tip.",
     "demonstrate":"Mark didn't demonstrate that Zoe calculated the tip.",
     "acknowledge":"Mark didn't acknowledge that Zoe calculated the tip.",
     "admit":"Mark didn't admit that Zoe calculated the tip.",
     "announce":"Mark didn't announce that Zoe calculated the tip.",
     "confess":"Mark didn't confess that Zoe calculated the tip.",
     "confirm":"Mark didn't confirm that Zoe calculated the tip.",
     "establish":"Mark didn't establish that Zoe calculated the tip.",
     "hear":"Mark didn't hear that Zoe calculated the tip.",
     "inform":"Mark didn't inform Sam that Zoe calculated the tip.",
     "prove":"Mark didn't prove that Zoe calculated the tip."
   },
   "danny": {
     "question":"Danny ate the last cupcake",
     "MC":"Did Danny eat the last cupcake.",
     "be_annoyed":"Kathryn isn't annoyed that Danny ate the last cupcake.",
     "discover":"Kathryn didn't discover that Danny ate the last cupcake.",
     "know":"Kathryn doesn't know that Danny ate the last cupcake.",
     "reveal":"Kathryn didn't reveal that Danny ate the last cupcake.",
     "see":"Kathryn didn't see that Danny ate the last cupcake.",
     "pretend":"Kathryn didn't pretend that Danny ate the last cupcake.",
     "suggest":"Kathryn didn't suggest that Danny ate the last cupcake.",
     "say":"Kathryn didn't say that Danny ate the last cupcake.",
     "think":"Kathryn doesn't think that Danny ate the last cupcake.",
     "be_right":"Kathryn isn't right that Danny ate the last cupcake.",
     "demonstrate":"Kathryn didn't demonstrate that Danny ate the last cupcake.",
     "acknowledge":"Kathryn didn't acknowledge that Danny ate the last cupcake.",
     "admit":"Kathryn didn't admit that Danny ate the last cupcake.",
     "announce":"Kathryn didn't announce that Danny ate the last cupcake.",
     "confess":"Kathryn didn't confess that Danny ate the last cupcake.",
     "confirm":"Kathryn didn't confirm that Danny ate the last cupcake.",
     "establish":"Kathryn didn't establish that Danny ate the last cupcake.",
     "hear":"Kathryn didn't hear that Danny ate the last cupcake.",
     "inform":"Kathryn didn't inform Sam that Danny ate the last cupcake.",
     "prove":"Kathryn didn't prove that Danny ate the last cupcake."
   },
   "frank": {
     "question":"Frank got a cat",
     "MC":"Frank didn't get a cat.",
     "be_annoyed":"Walt isn't annoyed that Frank got a cat.",
     "discover":"Walt didn't discover that Frank got a cat.",
     "know":"Walt doesn't know that Frank got a cat.",
     "reveal":"Walt didn't reveal that Frank got a cat.",
     "see":"Walt didn't see that Frank got a cat.",
     "pretend":"Walt didn't pretend that Frank got a cat.",
     "suggest":"Walt didn't suggest that Frank got a cat.",
     "say":"Walt didn't say that Frank got a cat.",
     "think":"Walt doesn't think that Frank got a cat.",
     "be_right":"Walt isn't right that Frank got a cat.",
     "demonstrate":"Walt didn't demonstrate that Frank got a cat.",
     "acknowledge":"Walt didn't acknowledge that Frank got a cat.",
     "admit":"Walt didn't admit that Frank got a cat.",
     "announce":"Walt didn't announce that Frank got a cat.",
     "confess":"Walt didn't confess that Frank got a cat.",
     "confirm":"Walt didn't confirm that Frank got a cat.",
     "establish":"Walt didn't establish that Frank got a cat.",
     "hear":"Walt didn't hear that Frank got a cat.",
     "inform":"Walt didn't inform Sam that Frank got a cat.",
     "prove":"Walt didn't prove that Frank got a cat."
   },
   "jackson": {
     "question":"Jackson ran 10 miles",
     "MC":"Did Jackson run 10 miles.",
     "be_annoyed":"Randy isn't annoyed that Jackson ran 10 miles.",
     "discover":"Randy didn't discover that Jackson ran 10 miles.",
     "know":"Randy doesn't know that Jackson ran 10 miles.",
     "reveal":"Randy didn't reveal that Jackson ran 10 miles.",
     "see":"Randy didn't see that Jackson ran 10 miles.",
     "pretend":"Randy didn't pretend that Jackson ran 10 miles.",
     "suggest":"Randy didn't suggest that Jackson ran 10 miles.",
     "say":"Randy didn't say that Jackson ran 10 miles.",
     "think":"Randy doesn't think that Jackson ran 10 miles.",
     "be_right":"Randy isn't right that Jackson ran 10 miles.",
     "demonstrate":"Randy didn't demonstrate that Jackson ran 10 miles.",
     "acknowledge":"Randy didn't acknowledge that Jackson ran 10 miles.",
     "admit":"Randy didn't admit that Jackson ran 10 miles.",
     "announce":"Randy didn't announce that Jackson ran 10 miles.",
     "confess":"Randy didn't confess that Jackson ran 10 miles.",
     "confirm":"Randy didn't confirm that Jackson ran 10 miles.",
     "establish":"Randy didn't establish that Jackson ran 10 miles.",
     "hear":"Randy didn't hear that Jackson ran 10 miles.",
     "inform":"Randy didn't inform Sam that Jackson ran 10 miles.",
     "prove":"Randy didn't prove that Jackson ran 10 miles."
   },
   "jayden": {
     "question":"Jayden rented a car",
     "MC":"Did Jayden rent a car.",
     "be_annoyed":"Herbert isn't annoyed that Jayden rented a car.",
     "discover":"Herbert didn't discover that Jayden rented a car.",
     "know":"Herbert doesn't know that Jayden rented a car.",
     "reveal":"Herbert didn't reveal that Jayden rented a car.",
     "see":"Herbert didn't see that Jayden rented a car.",
     "pretend":"Herbert didn't pretend that Jayden rented a car.",
     "suggest":"Herbert didn't suggest that Jayden rented a car.",
     "say":"Herbert didn't say that Jayden rented a car.",
     "think":"Herbert doesn't think that Jayden rented a car.",
     "be_right":"Herbert isn't right that Jayden rented a car.",
     "demonstrate":"Herbert didn't demonstrate that Jayden rented a car.",
     "acknowledge":"Herbert didn't acknowledge that Jayden rented a car.",
     "admit":"Herbert didn't admit that Jayden rented a car.",
     "announce":"Herbert didn't announce that Jayden rented a car.",
     "confess":"Herbert didn't confess that Jayden rented a car.",
     "confirm":"Herbert didn't confirm that Jayden rented a car.",
     "establish":"Herbert didn't establish that Jayden rented a car.",
     "hear":"Herbert didn't hear that Jayden rented a car.",
     "inform":"Herbert didn't inform Sam that Jayden rented a car.",
     "prove":"Herbert didn't prove that Jayden rented a car."
   },
   "tony": {
     "question":"Tony had a drink last night",
     "MC":"Did Tony have a drink last night.",
     "be_annoyed":"Helen isn't annoyed that Tony had a drink last night.",
     "discover":"Helen didn't discover that Tony had a drink last night.",
     "know":"Helen doesn't know that Tony had a drink last night.",
     "reveal":"Helen didn't reveal that Tony had a drink last night.",
     "see":"Helen didn't see that Tony had a drink last night.",
     "pretend":"Helen didn't pretend that Tony had a drink last night.",
     "suggest":"Helen didn't suggest that Tony had a drink last night.",
     "say":"Helen didn't say that Tony had a drink last night.",
     "think":"Helen doesn't think that Tony had a drink last night.",
     "be_right":"Helen isn't right that Tony had a drink last night.",
     "demonstrate":"Helen didn't demonstrate that Tony had a drink last night.",
     "acknowledge":"Helen didn't acknowledge that Tony had a drink last night.",
     "admit":"Helen didn't admit that Tony had a drink last night.",
     "announce":"Helen didn't announce that Tony had a drink last night.",
     "confess":"Helen didn't confess that Tony had a drink last night.",
     "confirm":"Helen didn't confirm that Tony had a drink last night.",
     "establish":"Helen didn't establish that Tony had a drink last night.",
     "hear":"Helen didn't hear that Tony had a drink last night.",
     "inform":"Helen didn't inform Sam that Tony had a drink last night.",
     "prove":"Helen didn't prove that Tony had a drink last night."
   },
   "josh": {
     "question":"Josh learned to ride a bike yesterday",
     "MC":"Did Josh learn to ride a bike yesterday.",
     "be_annoyed":"Brad isn't annoyed that Josh learned to ride a bike yesterday.",
     "discover":"Brad didn't discover that Josh learned to ride a bike yesterday.",
     "know":"Brad doesn't know that Josh learned to ride a bike yesterday.",
     "reveal":"Brad didn't reveal that Josh learned to ride a bike yesterday.",
     "see":"Brad didn't see that Josh learned to ride a bike yesterday.",
     "pretend":"Brad didn't pretend that Josh learned to ride a bike yesterday.",
     "suggest":"Brad didn't suggest that Josh learned to ride a bike yesterday.",
     "say":"Brad didn't say that Josh learned to ride a bike yesterday.",
     "think":"Brad doesn't think that Josh learned to ride a bike yesterday.",
     "be_right":"Brad isn't right that Josh learned to ride a bike yesterday.",
     "demonstrate":"Brad didn't demonstrate that Josh learned to ride a bike yesterday.",
     "acknowledge":"Brad didn't acknowledge that Josh learned to ride a bike yesterday.",
     "admit":"Brad didn't admit that Josh learned to ride a bike yesterday.",
     "announce":"Brad didn't announce that Josh learned to ride a bike yesterday.",
     "confess":"Brad didn't confess that Josh learned to ride a bike yesterday.",
     "confirm":"Brad didn't confirm that Josh learned to ride a bike yesterday.",
     "establish":"Brad didn't establish that Josh learned to ride a bike yesterday.",
     "hear":"Brad didn't hear that Josh learned to ride a bike yesterday.",
     "inform":"Brad didn't inform Sam that Josh learned to ride a bike yesterday.",
     "prove":"Brad didn't prove that Josh learned to ride a bike yesterday."
   },
   "owen": {
     "question":"Owen shoveled snow last winter",
     "MC":"Did Owen shovel snow last winter.",
     "be_annoyed":"Jordan isn't annoyed that Owen shoveled snow last winter.",
     "discover":"Jordan didn't discover that Owen shoveled snow last winter.",
     "know":"Jordan doesn't know that Owen shoveled snow last winter.",
     "reveal":"Jordan didn't reveal that Owen shoveled snow last winter.",
     "see":"Jordan didn't see that Owen shoveled snow last winter.",
     "pretend":"Jordan didn't pretend that Owen shoveled snow last winter.",
     "suggest":"Jordan didn't suggest that Owen shoveled snow last winter.",
     "say":"Jordan didn't say that Owen shoveled snow last winter.",
     "think":"Jordan doesn't think that Owen shoveled snow last winter.",
     "be_right":"Jordan isn't right that Owen shoveled snow last winter.",
     "demonstrate":"Jordan didn't demonstrate that Owen shoveled snow last winter.",
     "acknowledge":"Jordan didn't acknowledge that Owen shoveled snow last winter.",
     "admit":"Jordan didn't admit that Owen shoveled snow last winter.",
     "announce":"Jordan didn't announce that Owen shoveled snow last winter.",
     "confess":"Jordan didn't confess that Owen shoveled snow last winter.",
     "confirm":"Jordan didn't confirm that Owen shoveled snow last winter.",
     "establish":"Jordan didn't establish that Owen shoveled snow last winter.",
     "hear":"Jordan didn't hear that Owen shoveled snow last winter.",
     "inform":"Jordan didn't inform Sam that Owen shoveled snow last winter.",
     "prove":"Jordan didn't prove that Owen shoveled snow last winter."
   },
   "julian": {
     "question":"Julian dances salsa",
     "MC":"Does Julian dance salsa.",
     "be_annoyed":"Cole isn't annoyed that Julian dances salsa.",
     "discover":"Cole didn't discover that Julian dances salsa.",
     "know":"Cole doesn't know that Julian dances salsa.",
     "reveal":"Cole didn't reveal that Julian dances salsa.",
     "see":"Cole didn't see that Julian dances salsa.",
     "pretend":"Cole didn't pretend that Julian dances salsa.",
     "suggest":"Cole didn't suggest that Julian dances salsa.",
     "say":"Cole didn't say that Julian dances salsa.",
     "think":"Cole doesn't think that Julian dances salsa.",
     "be_right":"Cole isn't right that Julian dances salsa.",
     "demonstrate":"Cole didn't demonstrate that Julian dances salsa.",
     "acknowledge":"Cole didn't acknowledge that Julian dances salsa.",
     "admit":"Cole didn't admit that Julian dances salsa.",
     "announce":"Cole didn't announce that Julian dances salsa.",
     "confess":"Cole didn't confess that Julian dances salsa.",
     "confirm":"Cole didn't confirm that Julian dances salsa.",
     "establish":"Cole didn't establish that Julian dances salsa.",
     "hear":"Cole didn't hear that Julian dances salsa.",
     "inform":"Cole didn't inform Sam that Julian dances salsa.",
     "prove":"Cole didn't prove that Julian dances salsa."
   },
   "jon": {
     "question":"Jon walks to work",
     "MC":"Does Jon walk to work.",
     "be_annoyed":"Dexter isn't annoyed that Jon walks to work.",
     "discover":"Dexter didn't discover that Jon walks to work.",
     "know":"Dexter doesn't know that Jon walks to work.",
     "reveal":"Dexter didn't reveal that Jon walks to work.",
     "see":"Dexter didn't see that Jon walks to work.",
     "pretend":"Dexter didn't pretend that Jon walks to work.",
     "suggest":"Dexter didn't suggest that Jon walks to work.",
     "say":"Dexter didn't say that Jon walks to work.",
     "think":"Dexter doesn't think that Jon walks to work.",
     "be_right":"Dexter isn't right that Jon walks to work.",
     "demonstrate":"Dexter didn't demonstrate that Jon walks to work.",
     "acknowledge":"Dexter didn't acknowledge that Jon walks to work.",
     "admit":"Dexter didn't admit that Jon walks to work.",
     "announce":"Dexter didn't announce that Jon walks to work.",
     "confess":"Dexter didn't confess that Jon walks to work.",
     "confirm":"Dexter didn't confirm that Jon walks to work.",
     "establish":"Dexter didn't establish that Jon walks to work.",
     "hear":"Dexter didn't hear that Jon walks to work.",
     "inform":"Dexter didn't inform Sam that Jon walks to work.",
     "prove":"Dexter didn't prove that Jon walks to work."
   },
   "charley": {
     "question":"Charley speaks Spanish",
     "MC":"Does Charley speak Spanish.",
     "be_annoyed":"Anton isn't annoyed that Charley speaks Spanish.",
     "discover":"Anton didn't discover that Charley speaks Spanish.",
     "know":"Anton doesn't know that Charley speaks Spanish.",
     "reveal":"Anton didn't reveal that Charley speaks Spanish.",
     "see":"Anton didn't see that Charley speaks Spanish.",
     "pretend":"Anton didn't pretend that Charley speaks Spanish.",
     "suggest":"Anton didn't suggest that Charley speaks Spanish.",
     "say":"Anton didn't say that Charley speaks Spanish.",
     "think":"Anton doesn't think that Charley speaks Spanish.",
     "be_right":"Anton isn't right that Charley speaks Spanish.",
     "demonstrate":"Anton didn't demonstrate that Charley speaks Spanish.",
     "acknowledge":"Anton didn't acknowledge that Charley speaks Spanish.",
     "admit":"Anton didn't admit that Charley speaks Spanish.",
     "announce":"Anton didn't announce that Charley speaks Spanish.",
     "confess":"Anton didn't confess that Charley speaks Spanish.",
     "confirm":"Anton didn't confirm that Charley speaks Spanish.",
     "establish":"Anton didn't establish that Charley speaks Spanish.",
     "hear":"Anton didn't hear that Charley speaks Spanish.",
     "inform":"Anton didn't inform Sam that Charley speaks Spanish.",
     "prove":"Anton didn't prove that Charley speaks Spanish."
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
    var utterance = mcitems[j].MCq;
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