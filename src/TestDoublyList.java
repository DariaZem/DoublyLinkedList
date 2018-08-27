public class TestDoublyList {
    public static void main(String[] args) {
        DoublyList link = new DoublyList();
        DoublyList listA = link.view(25, 26, 27, 28);
        link.addLast(100,listA);
        link.addInPosition(50,3,listA);
        link.removeLast(listA);
        link.removeByPosition(0, listA,link.size(listA));
        System.out.println(link.toString(link.getTail()));
        System.out.println(link.isGrowing(listA,link.size(listA)));
    }
}
