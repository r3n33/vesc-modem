
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.3
import Vedder.vesc.utility 1.0

import Vedder.vesc.commands 1.0

Item {
    id: mainItem
    anchors.fill: parent
    anchors.margins: 5

    property Commands mCommands: VescIf.commands()

    function setAPN(apn) {
        mCommands.lispSendReplCmd("(apn-save \"" + apn + "\")")
    }

    Connections {
        target: mCommands

        function onCustomAppDataReceived(data) {
            txtAPN.text = data
            console.log(data)
        }
    }

    Component.onCompleted: {
        mCommands.lispSendReplCmd("(apn-load)")
    }

    Item {
        id: contentWrapper
        anchors.centerIn: parent
        width: column.implicitWidth
        height: column.implicitHeight

        ColumnLayout {
            id: column
            anchors.fill: parent
            spacing: 10

            TextArea {
                id: txtAPN
                wrapMode: Text.NoWrap
                placeholderText: "Enter your APN here..."
                font.pointSize: 12
                selectByMouse: true
                text: "iot.1nce.net"
                Layout.alignment: Qt.AlignHCenter
                implicitWidth: 200
                implicitHeight: 40

                onTextChanged: {
                    console.log("Text changed:", text)
                }
            }

            Button {
                text: "Save APN"
                Layout.alignment: Qt.AlignHCenter
                onClicked: {
                    setAPN(txtAPN.text)
                }
            }

            Button {
                text: "Load APN"
                Layout.alignment: Qt.AlignHCenter
                onClicked: {
                    mCommands.lispSendReplCmd("(apn-load)")
                }
            }
        }
    }
}
