$(document).ready(function() {

	$("#files .preview a").click(function() {

		// Toggle 'show' and 'hide'
		if ($(this).html() == "show") {
			$(this).html("hide");
		}
		else {
			$(this).html("show");
		}
		var target = $(this).closest("tr");

		if (target.next().hasClass("preview")) {
			target.next().toggle();
		}
		else {
			var url = target.find('a').attr('href');
			// Fetch the file contents from the given url
			$.get(url, function(ret) {

			}).complete(function(jqXHR, textStatus) {
				// When request has completed,
				// let's display the contents depending on the content-type
				var contentType = jqXHR.getResponseHeader('Content-Type');
				var data = jqXHR.responseText;
				// If it's an image, display content in an <img> tag
				if (contentType.split('/')[0] == "image") {
					var img = '<img src="' + url + '" />';
					target.after('<tr class="preview"><td colspan="5">' + img + '</td></tr>');
				}
				// Otherwise, print contents
				else {
					switch (contentType) {
						case "text/plain":
							output = data;
							break;
						default:
							output = "Preview for this filetype not supported yet. Click the filename to preview in a new browser tab.";
							break;
					}
					// Make sure newlines are printed
					output = output.replace(/\n/g, "<br>");
					// Insert the data
					target.after('<tr class="preview"><td colspan="5">' + output + '</td></tr>');
				}
			});
		}
	}); // End #files click

	var dropbox = $('#dropbox'),
			message = $('.message', dropbox);

	dropbox.filedrop({
		// The name of the files array
		paramname: 'files',

		maxfiles: 5,
		maxfilesize: 5, // in megabytes
		url: dropbox.attr('data-target'),

		// Set a lighter border-color when we drag a file over the dropbox
		dragOver: function() {
			dropbox.css('border-color', '#ccc');
		},

		// Return to normal after we leave the dropbox
		dragLeave: function() {
			dropbox.css('border-color', '#000');
		},
		drop: function() {
			dropbox.css('border-color', '#000');
		},

		// file is a File object containing
		// 	lastModifiedDate, name, size, webkitRelativePath
		// response is the path to the temporarily stored file

		uploadFinished: function(i, file, response) {
			switch (response.status) {
				case "ok":
					$.data(file).find(".status").html('<i class="icon-ok"></i> ok!');
					break;
				case "error":
					$.data(file).find(".status").html('<i class="icon-warning-sign"></i> error!');
					// Todo, include response.message
					break;
			}
		},

		error: function(err, file) {
			switch (err) {
				case 'BrowserNotSupported':
					showMessage('Your browser does not support HTML5 file uploads. Why not give Chrome or Firefox a try?');
					break;
				case 'TooManyFiles':
					showWarning('Too many files! Please select 5 at most!');
					break;
				case 'FileTooLarge':
					showWarning('\'' + file.name + '\' is too large! Max size allowed 5 MB');
					break;
				default:
					break;
			}
		},

		// TODO: show spinner? reject certain file types?
		// Called before each upload is started
		beforeEach: function(file) {
			// if(!file.type.match(/^image\//)){ return false; }
		},

		uploadStarted:function(i, file, len) {
			var preview = previewFile(file.name, file.size);
			// Bind the file data to this particular preview element
			$.data(file, preview);
		},

		progressUpdated: function(i, file, progress) {
			// $.data(file) contains the <tr> corresponding to this file's preview,
			// update the progress bar in that <tr>
			$.data(file).find('.bar').css('width', progress + '%');;
		},

		// TODO speed i kb/s
		speedUpdated: function(i, file, speed) {

    },
	});

	function showWarning(msg) {
		message.addClass('warning');
		message.html(msg);
	}

	function showMessage(msg){
		message.html(msg);
	}

	function previewFile(filename, size) {
		var target = $("#preview");
		var tbody = target.find("tbody");

		// Structure for the progress bar
		var progress =
		'<div class="progress progress-striped active">' +
  		'<div class="bar" style="width: 0%;"></div>' +
		'</div>'

		// Everything put together
		var template = '<tr>' +
			'<td class="filename">' + filename + '</td>' +
			'<td>' + fileSizeFormat(size) +'</td>' +
			'<td>' + progress +'</td>' +
			'<td class="status"></td>';
			'<tr>';

		// jQuery'alized
		var tr = $(template);
		// Add the row to the preview table's tbody
		tbody.append(tr);
		// Return it so we can bind some File data to it
		return tr;
	}

	// Format the value like a human-readable file size
	var FD_KILOBYTE = 1024;
	var FD_MEGABYTE = FD_KILOBYTE * 1024;
	var FD_GIGABYTE = FD_MEGABYTE * 1024;

	function fileSizeFormat(size) {
		if (size >= FD_GIGABYTE)
			return fileSizeFormat(size/FD_GIGABYTE) + " GB";
		else if (size >= FD_MEGABYTE)
			return fileSizeFormat(size/FD_MEGABYTE) + " MB";
		else if (size >= FD_KILOBYTE)
			return fileSizeFormat(size/FD_KILOBYTE) + " KB";
		else if (size.toString().indexOf(".") != -1)
			return Math.round(size*100)/100;
		else
			return size + " bytes";
	}

	$(document).on("click", ".delete a", function(event) {
		event.stopPropagation();
		console.log($(this));
		console.log("click!");
	});

});