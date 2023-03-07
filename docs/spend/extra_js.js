toggleF = function(id){
  
  var sel = '#' + id;
  
  $(sel).toggle();
}

picker_f = function(pick_id, pick_class){
  
  var t_sect = $('.' + pick_class);
  
  t_sect.each(function(index,value){
      
      $(value).hide();
    });
  
  var pick_val = $('#' + pick_id).val();
  
  var sel = '#' + pick_val;
  
  $(sel).toggle();
}

$(document).ready(
  function(){
    
    var sections = $('.sect');
    
    sections.each(function(index,value){
      
      $(value).hide();
    })
    
  }
)