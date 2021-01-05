Shiny.addCustomMessageHandler('setsetting', function(value) {
console.log(value);
Shiny.setInputValue(value[0], value.slice(1,value.length));
});

$(document).on('shiny:inputchanged', function(event) {
    console.log(event);
  });
