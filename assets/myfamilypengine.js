$(document).on("click", "#queryButton", function(event) {
    const query = $('#query').val();
    const queryText = `?- ${query}.`;
    console.log(queryText);
    $("#out").text("");
    $('<div></div>').text(queryText).appendTo("#out");
    const pengine = createPengine(query);
    // pengine.ask(query, {chunk:10});
});

function createPengine(query) {
    const pengine = new Pengine({
        oncreate: function () {
            console.log("Created");
            pengine.ask(query, {chunk:10});
        },
        onprompt: function () {
            pengine.input(prompt(this.data));
        },
        onoutput: function () {
            console.log(this.data);
        },
        onsuccess: function () {
            this.data.forEach(result => {
                const resultElement = $('<div class="rslt"></div>');
                for (const key in result) {
                    if (result.hasOwnProperty(key)) {
                        const value = result[key];
                        const keyValueText = `${key} = ${value}`;
                        const variableElement = $('<span class="vel"></span>');
                        console.log(keyValueText);
                        variableElement.text(keyValueText);
                        resultElement.append(variableElement);
                    }
                }
                resultElement.appendTo("#out");
            });
            if (this.more) {
                console.log('Next...');
                pengine.next();
            } else {
                console.log('Done.');
                $("<div>.</div>").appendTo("#out");
            }
        }
    });
    return pengine;
}
